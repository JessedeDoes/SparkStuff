
// http://slick.lightbend.com/doc/3.0.0/sql.html
// http://queirozf.com/entries/slick-3-reference-and-examples
// https://github.com/slick/slick/issues/1161 voor samenstellen queries

import slick.driver.H2Driver.api._
import java.util.concurrent.Executors
import scala.concurrent._
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.forkjoin._
import org.apache.log4j.Logger
import org.apache.log4j.Level
import slick.jdbc.GetResult
import slick.jdbc.{SQLActionBuilder,SetParameter,PositionedParameters}
import scala.util.{Try, Success, Failure}

case class Lemma(modern_lemma: String, lemma_id:Int, pos:String) 
case class Wordform(lemma: Lemma, analyzed_wordform_id:Int, wordform: String)
case class Attestation(wordform: Wordform, quote:String, hitStart: Int, hitEnd: Int)

object queries
{
    implicit def l(s:String,i:Int,s1:String):Lemma = Lemma(s,i,s1)
    
    val pos = "ADP"
    
    val lemmaQuery = 
    {
      implicit val getLemma = GetResult[Lemma](r => Lemma(r.nextString, r.nextInt, r.nextString))
      sql"""
      select modern_lemma, lemma_id,lemma_part_of_speech from data.lemmata 
        where lemma_part_of_speech ~ ${pos}""".as[Lemma]
    }
    
    def lemmaQueryWhere(where:String) = 
    {
      implicit val getLemma = GetResult[Lemma](r => Lemma(r.nextString, r.nextInt, r.nextString()))
      sql"""
      select modern_lemma, lemma_id,lemma_part_of_speech from data.lemmata 
        where #${where}""".as[Lemma]
    }
    
    def getWordforms(lemmata: List[Lemma]) = 
    {
      val ids = lemmata.map(_.lemma_id)
      val lemmaMap = lemmata.map(l => (l.lemma_id,l)).toMap
      implicit val makeWordform = GetResult[Wordform](
          r => Wordform(lemmaMap(r.nextInt), r.nextInt, r.nextString)
      )
      concat(sql"""select lemma_id, analyzed_wordform_id, wordform from data.analyzed_wordforms a, data.wordforms w 
                   where a.wordform_id=w.wordform_id and lemma_id in """, values(ids)).as[Wordform]
    }
    
    val testQuery = sql""" select 42 """.as[Int]
    
    val wfQuery = sql"""
select modern_lemma, l.lemma_id,lemma_part_of_speech, analyzed_wordform_id, wordform
     from data.lemmata l, data.analyzed_wordforms a, data.wordforms w
where
     l.lemma_id=a.lemma_id
     AND w.wordform_id=a.wordform_id
     AND lemma_part_of_speech ~  'ADP'"""
  
  val attestationQuery = sql"""
select 
    modern_lemma, l.lemma_id,lemma_part_of_speech, a.analyzed_wordform_id, wordform,
      quote, start_pos,end_pos
     from data.lemmata l, data.analyzed_wordforms a, data.wordforms w, data.token_attestations t
where
     l.lemma_id=a.lemma_id
     AND w.wordform_id=a.wordform_id
     AND a.analyzed_wordform_id=t.analyzed_wordform_id"""
  
  
  def concat(a: SQLActionBuilder, b: SQLActionBuilder): SQLActionBuilder = 
  {
		SQLActionBuilder(a.queryParts ++ b.queryParts, new SetParameter[Unit] {
				def apply(p: Unit, pp: PositionedParameters): Unit = {
						a.unitPConv.apply(p, pp)
						b.unitPConv.apply(p, pp)
				}
		})
  }
    
   def values[T](xs: TraversableOnce[T])(implicit sp: SetParameter[T]): SQLActionBuilder = {
      var b = sql"("
      var first = true
      xs.foreach { x =>
        if(first) first = false
        else b = concat(b, sql",")
        b = concat(b, sql"$x")
      }
      concat(b, sql")")
    }
   
   // as in: concat(sql"select id from USERS where id in ", values(Seq(1,2))).as[Int]
}

object Hilex 
{
  Logger.getRootLogger.setLevel(Level.ERROR)
  
  implicit lazy val ec = new ExecutionContext
  {
    val threadPool = Executors.newFixedThreadPool(5);

    def execute(runnable: Runnable)
    {
      threadPool.submit(runnable)
    }

    def reportFailure(t: Throwable) {}
  }

  val db = 
    Database.forURL("jdbc:postgresql://svowdb06/gigant_hilex?user=fannee&password=Cric0topus", driver="org.postgresql.Driver")

  def test =
  {
    println("hallo")
    val s = db.stream(queries.lemmaQuery)
    println(s)
    val r = s.foreach(x => println(x))
    Await.result(r, 10.seconds)
    println("done")
  }
  
  def slurp = (a:DBIOAction[_,_,_]) => { val r = db.run(a); r}
  
    
  
  def findSomeLemmata:Vector[Lemma] =
  {
    val r = db.run(queries.lemmaQueryWhere("lemma_part_of_speech ~ 'INT'"))
          
    Await.result(r, 120.seconds)
    println(r.value)
    r.value match
    {
      case Some(Success(x)) => x
    }
  }
  
  def main(args:Array[String]):Unit = 
  { 
    val l = findSomeLemmata
    val q = queries.getWordforms(l.toList)
  }
}
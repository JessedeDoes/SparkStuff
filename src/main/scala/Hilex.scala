
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
import slick.jdbc.{SQLActionBuilder,SetParameter,PositionedParameters, PositionedResult}
import scala.util.{Try, Success, Failure}
import scala.reflect._

case class Lemma(modern_lemma: String, lemma_id:Int, persistent_id:String, pos:String) 
{
   lazy val wordforms = Hilex.slurp(queries.getWordforms(List(this)))
   lazy val senses = Hilex.slurp(queries.getSenses(List(this)))
}

case class Wordform(lemma: Lemma, analyzed_wordform_id:Int, wordform: String)

case class Attestation(wordform: Wordform, quote:String, start_pos: Int, end_pos: Int, eg_id: Option[String])
{
   lazy val senses = ???
   import TokenizerWithOffsets._
   import entities._
   toConcordance
   
   def sillyCriterium (x:TokenWithOffsets):Int = 
   {
       x match { case TokenWithOffsets(t,s,e) => Math.abs(s - start_pos + e - end_pos) } 
   }
   
   def toConcordance =
   {
     val tokens = TokenizerWithOffsets.tokenize(quote)(really=false)
     val probableHit = tokens.toList.zipWithIndex.minBy(x => sillyCriterium(x._1))
     //println(probableHit)
     val tokenStream = tokens.toStream
     
     val c = new Concordance(probableHit._2, probableHit._2+1, List(("word",tokens.map( t =>  entities.substitute(t.token.token) ))).toMap, Map.empty)
     c
     //println(c.toString())
     // hm... 
   }
}

case class Sense(lemma: Lemma, persistent_id: String, lemma_id:String, parent_id: String, definition: String)
{
  lazy val attestations = Hilex.slurp(queries.getAttestationsForSense(List(this)))
  lazy val parentSense = ???
  lazy val subSenses = ???
}

private object util
{
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
}

object queries
{   
  
    import util._
    val pos = "ADP"
    
    val dataSchema = if (TestSpark.atHome) "data" else "gigant_hilex_data"
    val senseSchema = "wnt_ids"
    
    val lemmaQuery = 
    {
      implicit val getLemma = GetResult[Lemma](r => Lemma(r.nextString, r.nextInt, r.nextString,r.nextString))
      sql"""
      select modern_lemma, lemma_id,persistent_id, lemma_part_of_speech from data.lemmata 
        where lemma_part_of_speech ~ ${pos}""".as[Lemma]
    }
    
    def lemmaQueryWhere(where:String) = 
    {
      implicit val getLemma = GetResult[Lemma](r => Lemma(r.nextString, r.nextInt, r.nextString,r.nextString))
      sql"""
      select modern_lemma, lemma_id,persistent_id, lemma_part_of_speech from #${dataSchema}.lemmata 
        where #${where}""".as[Lemma]
    }
    
    def getWordforms(lemmata: List[Lemma]) = 
    {
      val ids = lemmata.map(_.lemma_id)
      val lemmaMap = lemmata.map(l => (l.lemma_id,l)).toMap
      implicit val makeWordform = GetResult[Wordform](
          r => Wordform(lemmaMap(r.nextInt), r.nextInt, r.nextString)
      )
      concat(sql"""select lemma_id, analyzed_wordform_id, wordform from #${dataSchema}.analyzed_wordforms a, #${dataSchema}.wordforms w 
                   where a.wordform_id=w.wordform_id and lemma_id in """, values(ids)).as[Wordform]
    }
     
     def getSenses(lemmata: List[Lemma]) = 
     {
      val ids = lemmata.map(_.persistent_id)
      val lemmaMap = lemmata.map(l => (l.persistent_id,l)).toMap
      println(lemmaMap.toList)
      implicit val makeSense = GetResult[Sense](
          r => Sense(lemmaMap(r.nextString), r.nextString, r.nextString, r.nextString, r.nextString)
      )
      concat(sql"""select lemma_id, persistent_id, lemma_id, parent_sense_id , definition
                   from #${senseSchema}.senses 
                   where lemma_id in """, values(ids)).as[Sense]
     }
    
    
    def getAttestations(wordforms: List[Wordform]) =
    {
      val ids = wordforms.map(_.analyzed_wordform_id)
      val wordformMap = wordforms.map(l => (l.analyzed_wordform_id,l)).toMap
      implicit val makeAttestation =   GetResult[Attestation](
            r => Attestation(wordformMap(r.nextInt),r.nextString, r.nextInt, r.nextInt, Some(r.nextString)))
  
     concat(sql"""
      select 
           a.analyzed_wordform_id,
           quote, start_pos,end_pos, eg_id
      from
           #${dataSchema}.analyzed_wordforms a, #${dataSchema}.token_attestations t, wnt_ids.documents d
      where
             d.document_id=t.document_id
            and a.analyzed_wordform_id=t.analyzed_wordform_id 
            and t.analyzed_wordform_id in """, values(ids)).as[Attestation]
    } 
    
    def getLemma(lemma_id: Int):Lemma =
    {
        val l = Hilex.slurp(queries.lemmaQueryWhere(s"lemma_id='${lemma_id}'" ))
        if (l.isEmpty)
         null
       else
         l.head
    }
    
    def getWordform(analyzed_wordform_id: Int):Wordform = 
    {
      implicit val makeWordform = GetResult[Wordform](
          r => Wordform(getLemma(r.nextInt), r.nextInt, r.nextString)
      )
      val a = sql"""select lemma_id, analyzed_wordform_id, wordform 
                       from #${dataSchema}.analyzed_wordforms a, #${dataSchema}.wordforms w 
                   where a.wordform_id=w.wordform_id and analyzed_wordform_id=${analyzed_wordform_id} """.as[Wordform]
       val l:List[Wordform] = Hilex.slurp(a)
       if (l.isEmpty)
         null
       else
         l.head
       //if (l.
    }
    
    def getAttestationsForSense(senses: List[Sense]) =
    {
      val ids = senses.map(_.persistent_id)
      val senseMap = senses.map(l => (l.persistent_id,l)).toMap
      implicit val makeAttestation =   GetResult[Attestation](
            r => Attestation(getWordform(r.nextInt),r.nextString, r.nextInt, r.nextInt, Some(r.nextString)))
  
     concat(sql"""
      select 
           analyzed_wordform_id,
           quote, start_pos,end_pos, d.eg_id
      from
            #${dataSchema}.token_attestations t, #${senseSchema}.eg_sense e, #${senseSchema}.documents d
      where
            d.eg_id = e.eg_id
            and t.document_id=d.document_id
            and e.sense_id in """, values(ids)).as[Attestation]
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
  
  lazy val dbAtWork = 
    Database.forURL("jdbc:postgresql://svprre02/diamant_prototype?user=postgres&password=inl", driver="org.postgresql.Driver")
  lazy val dbAtHome = 
    Database.forURL("jdbc:postgresql://svowdb02/gigant_hilex_dev?user=postgres&password=inl", driver="org.postgresql.Driver")
   
  val db = if (TestSpark.atHome) dbAtHome else dbAtWork
  
  def test =
  {
 
    val s = db.stream(queries.lemmaQuery)
    
    val r = s.foreach(x => println(x))
    Await.result(r, 10.seconds)
    println("done")
  }
  
  
    def slurp[A] (a:DBIOAction[Vector[A], NoStream, Nothing]):List[A] =
    { 
       val r = db.run(a)
       Await.result(r,120.seconds)
       r.value match
      {
        case Some(Success(x)) => x.toList
       }
    }
  
    
  
  def findSomeLemmata:List[Lemma] =
  {
     slurp(queries.lemmaQueryWhere("modern_lemma ~ '^zin$'"))
  }
  
  def main(args:Array[String]):Unit = 
  { 
    val l = findSomeLemmata
    println("lemmata gevonden")
    l.foreach(println)
    println("betekenissen gevonden")
    l.foreach(x => x.senses.foreach(println))
    l.foreach(_.senses.foreach(s => 
      { println(s"\nAttestaties voor ${s}"); s.attestations.foreach( a => println(a.toConcordance)) }))
    val qs = queries.getSenses(l)
    /*
    val qsa = queries.getAttestationsForSense(slurp(qs))
    println("Attestaties van betekenissen?")
    slurp(qsa).foreach(println)
    println("Zoek woordvormen erbij")
    * 
    */
    val q = queries.getWordforms(l)
    val l1 = slurp(q)
    l1.foreach(println)
    val q2 = queries.getAttestations(l1)
    val l2 = slurp(q2)
    l2.foreach(println)
  }
}
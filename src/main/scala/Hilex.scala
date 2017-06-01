import doobie.imports._ 
import scalaz._, Scalaz._, scalaz.concurrent.Task
import scalaz.effect.IO
//import scala.reflect.runtime.universe.TypeTag
//import org.postgresql.util.PGobject

case class Lemma(modern_lemma: String, lemma_id:Int, pos:String) 
case class Wordform(lemma: Lemma, analyzed_wordform_id:Int, wordform: String)

object Hilex 
{
  // dbc:postgresql://svowdb06/gig_pro?user=fannee&password=Cric0topus
  val atHome = false
  case class Country(code: String, name: String, population: Long)
  lazy val  xaWork = DriverManagerTransactor[IO](
				  "org.postgresql.Driver", "jdbc:postgresql://svowdb06/gigant_hilex", "fannee", "Cric0topus"
				  )
	lazy val xaHome = DriverManagerTransactor[IO](
				  "org.postgresql.Driver", "jdbc:postgresql://localhost/gigant_hilex_dev", "postgres", "inl"
				  )
  lazy val xa = if (atHome) xaHome else xaWork
  
  import xa.yolo._
  val wfQuery = sql"""
      select modern_lemma, l.lemma_id,lemma_part_of_speech, analyzed_wordform_id, wordform
     from data.lemmata l, data.analyzed_wordforms a, data.wordforms w
    where
     l.lemma_id=a.lemma_id
     AND w.wordform_id=a.wordform_id
AND
lemma_part_of_speech ~  'ADP'"""
  
  def testje =
  {
   


    val program2 = sql"""select modern_lemma, lemma_id,lemma_part_of_speech
        from data.lemmata where lemma_part_of_speech ~  'CON'""".query[Lemma].list
    val task2 = program2.transact(xa)
    println(task2.unsafePerformIO)
  //def find(n: String): ConnectionIO[Option[Country]] =
  //sql"select code, name, population from country where name = $n".query[Country].option

  // And then
  //find("France").transact(xa).unsafePerformIO
  }
  
  def test2 = 
  {
    val program2 = wfQuery.query[Wordform].list
    val task2 = program2.transact(xa)
    println(task2.unsafePerformIO)
  }
    
   def main(args:Array[String]):Unit = test2
     
}
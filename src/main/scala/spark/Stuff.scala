package spark

import concordance.Concordance
import corpus.Concordancer
import nl.inl.blacklab.search.Searcher
import org.apache.spark.SparkContext
import org.apache.spark.sql._
import org.apache.spark.sql.functions._

object withSpark {
  lazy val sparkSession: SparkSession = SparkSession.builder
    .master("local")
    .appName("My App")
    .getOrCreate()

  //val sc0 = new SparkContext(conf)
  lazy val sc: SparkContext = sparkSession.sparkContext

  def testBlacklabQuery(searcher: Searcher): DataFrame = {
    val c = new Concordancer(searcher)
    val concs:Stream[Concordance] = c.concordances("[pos='AA.*'][lemma='gezindheid']")
    val df = ConcordanceDataFrame.collectConcordances(concs, sparkSession)

    for {conc <-
         df.filter("pos[hitStart-1]='ADV()' and pos[hitEnd]='ADV()'").sort(desc("date")).selectExpr("date", "word", "lemma[hitStart] as lemma")} {
      // println(conc.getAs[String]("date") + " <"  + conc.getAs[String]("lemma") + "> " + conc.getAs[Array[_]]("word"))
    }
    df
  }
  def lemmaJoin(concordances: DataFrame, lemmaSet: DataFrame): DataFrame = {
    val joinedDF = concordances.join(lemmaSet, concordances("lemma") === lemmaSet("modern_lemma"), "inner")
    joinedDF
  }

  def testjeMetSparkJoin() = {
    //Log.set(Log.LEVEL_ERROR)

    val indexDirectory = if (configuration.Configuration.atHome) "/media/jesse/Data/Diamant/CorpusEzel/" else "/datalokaal/Corpus/BlacklabServerIndices/StatenGeneraal/"
    val searcher = Searcher.open(new java.io.File(indexDirectory))
    println("searcher open...")
    val concordances = testBlacklabQuery(searcher).selectExpr("date", "word", "lemma[hitStart] as lemma", "pos[hitStart] as pos")
    val lemmata = TestSpark.lemmataDataFrame(sc).filter("not (lemma_gigpos rlike 'AA')")
    val joined = lemmaJoin(concordances, lemmata)
    for (x <- joined)
      println(x)
  }

  def main(args: Array[String]): Unit = {

   testjeMetSparkJoin()
  }
  /*
def createSchema(hits: Hits, metaFields: List[String]): StructType = {
  val kwic = hits.getKwic(hits.get(0))

  val fields = kwic.getProperties.asScala.toList

  val tokenFields = fields.map(StructField(_, new ArrayType(StringType, false), nullable = true))
  val metaFieldz = metaFields.map(StructField(_, StringType, nullable = true))
  val extraFields = List("hitStart", "hitEnd").map(StructField(_, IntegerType, nullable = false))

  val schema = StructType(extraFields ++ tokenFields ++ metaFieldz)
  println("Schema:" + schema)
  schema
}


def createRow(kwic: Kwic, meta: Map[String, String]): Row = {
  val tokenValues = kwic.getProperties.asScala.toList.map(
    s => (kwic.getLeft(s).asScala.toList ++ kwic.getMatch(s).asScala.toList
      ++ kwic.getRight(s).asScala.toList).toArray);

  val metaKeys = meta.keys.toList.sorted
  val metaValues = metaKeys.map(s => meta(s))
  Row.fromSeq(kwic.getHitStart :: kwic.getHitEnd :: tokenValues ++ metaValues)
}
*/
}



/*
* Problemen als je wil paralleliseren: lucene query is niet puur of doet iedere keer nieuwe initialisatie
* 
* Oplossing ?! initialisatie een keer per node zie
* Zie http://stackoverflow.com/questions/40435741/object-cache-on-spark-executors?rq=1 (tweede manier)
* Maar dan wil je de grotere corpora maar op 1 node laden, dus
* zou je expliciet willen mappen welke deelvraag waarheen gaat, wat niet echt in de geest van het hele gespark is (?)  
*/

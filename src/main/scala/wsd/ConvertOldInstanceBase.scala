package wsd

import concordance.Concordance
import org.apache.log4j.{Level, Logger}
import org.apache.spark.sql._
import org.apache.spark.sql.types._

import scala.collection.JavaConverters._

object ConvertOldInstanceBase {
  lazy val sparkSession: SparkSession = SparkSession.builder
    .master("local")
    .appName("My App")
    .getOrCreate()

  //val sc0 = new SparkContext(conf)
  lazy val sc = sparkSession.sparkContext
  val tokenFields = List("word", "lemma", "pos")


  def uuid = java.util.UUID.randomUUID.toString

  def makeSchema(fields: List[String]): StructType = {
    val metaFields = List("senseId", "lempos", "id")
    val tokenFields = fields.map(StructField(_, new ArrayType(StringType, false), nullable = true))
    val metaFieldz = metaFields.map(StructField(_, StringType, nullable = true))
    val extraFields = List("hitStart", "hitEnd").map(StructField(_, IntegerType, nullable = false))

    val schema = StructType(extraFields ++ (tokenFields ++ metaFieldz))
    schema
  }

  val schema = makeSchema(tokenFields)

  def convert(fileName: String, session: SparkSession): DataFrame = {
    val w: WSDInstanceBase = WSDInstanceBase.loadFromFile(fileName)
    val rows =
      for (e <- w.entrySet().asScala;
           i <- e.getValue.asScala.map(x => convertInstance(x, e.getKey)))
        yield i
    session.createDataFrame(session.sparkContext.parallelize(rows.toList, 4), schema)
  }

  def convertToConcordances(fileName: String): List[Concordance] = {
    val w: WSDInstanceBase = WSDInstanceBase.loadFromFile(fileName)
    val rows =
      for (e <- w.entrySet().asScala;
           i <- e.getValue.asScala.map(x => instanceToConcordance(x, e.getKey)))
        yield i
    rows.toList
  }

  def instanceToConcordance(i: WSDInstance, lempos: String): Concordance = {
    val t = i.tokens.asScala

    val lemmata = t.map(_ getLemma).toArray
    val words = t.map(_ getWord).toArray
    val pos = t.map(_ getPoS).toArray

    val tokenFieldMap = List((lemmata, "lemma"), (words, "word"), (pos, "pos")).map({ case (x, y) => y -> x }).toMap
    val meta = Map(("senseId" -> i.senseId), ("lempos", lempos), ("id", uuid))
    Concordance(i.targetPosition, i.targetPosition + 1, tokenFieldMap, meta)
  }

  def convertInstance(i: WSDInstance, lempos: String): Row = {
    val t = i.tokens.asScala

    val lemmata = t.map(_ getLemma).toArray
    val words = t.map(_ getWord).toArray
    val pos = t.map(_ getPoS).toArray

    val tokenFieldMap = List((lemmata, "lemma"), (words, "word"), (pos, "pos")).map({ case (x, y) => y -> x }).toMap
    val tokenValues = tokenFields.map(x => tokenFieldMap(x))
    Row.fromSeq(i.targetPosition :: i.targetPosition + 1 :: tokenValues ++ List(i.senseId, lempos, uuid))
  }

  def saveToXML(concordances: Seq[Concordance], fileName: String): Unit = {
    val pw = new java.io.PrintWriter(new java.io.File(fileName))
    pw.write("<Snippets>\n")
    concordances.foreach(c => pw.write(c.toXML + "\n"))
    pw.write("</Snippets>\n")
    Console.err.println("converted..")
    pw.close
  }

  def main(args: Array[String]): Unit = {
    /*
    lazy val sparkSession:SparkSession = SparkSession.builder
         .master("local")
         .appName("My App")
         .getOrCreate()
         *
         */
    Logger.getLogger("org").setLevel(Level.WARN)
    Logger.getLogger("akka").setLevel(Level.WARN)
    Logger.getRootLogger.setLevel(Level.WARN)
    //val frames = convert(args(0), sparkSession)
    val concordances = convertToConcordances(args(0))
    Console.err.println("loaded..")

    saveToXML(concordances, "Data/instances.xml")
    Console.err.println("Instance based loaded")

    // tester.leaveOneOut(new wsd.Swsd, concordances)
    //frames.write.format("parquet").save("Data/wsdInstanceBase.parquet") // SaveMode.Overwrite
    //frames.rdd.saveAsTextFile("Data/aapje.framez")
  }
}

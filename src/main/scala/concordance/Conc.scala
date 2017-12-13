package concordance

import corpus.Concordancer
import corpus.Concordancer.corpusSize
import diamant.DictionaryWSD
import nl.inl.blacklab.search.Searcher
import spark.TestSpark
import wsd.{ConvertOldInstanceBase, tester, wsdObject}
import scala.collection.JavaConverters._

object Conc {
  import Concordancer._



  def collocationExample(searcher: Searcher) = {
    val concordancer = Concordancer(searcher)
    val q0 = "[lemma='zin' & word='(?c)zin' & pos='N.*']"
    val c0 = concordancer.concordances(q0) // dit is veel te langzaam zoals ik het doe. Hoe komt dat?

    val f1 = c0.count((x => true))
    println(s"Hits for ${q0} : ${f1}")


    val scored = concordancer.collocations(c0)

    for (s <- scored.sortWith({ case (a, b) => a._4 < b._4 }))
      println(s)
  }

  def wsdTest(searcher: Searcher) = {
    val concordancer = Concordancer(searcher)
    val in = concordancer.concordances("[word='ezel']").map(c => c.copy(metadata = c.metadata + ("id" -> ConvertOldInstanceBase.uuid)))
    val cw = wsdObject.tag(in, DictionaryWSD.ezelaar).map(DictionaryWSD.flattenEzel)
    cw.foreach(c => println(c.metadata.get("senseId") + "\t" + c))
  }

  def wsdTestZin(searcher: Searcher) = {
    val concordancer = Concordancer(searcher)
    val in = concordancer.concordances("[word='zin']").map(c => c.copy(metadata = c.metadata + ("id" -> ConvertOldInstanceBase.uuid)))
    val cw = wsdObject.tag(in, DictionaryWSD.bezinner).map(DictionaryWSD.flattenZin)
    cw.foreach(c => println(c.metadata.get("senseId") + "\t" + c))
  }

  val corpusEzel = "/media/jesse/Data/Diamant/CorpusEzel/"
  val corpusWolf = "/media/jesse/Data/Diamant/CorpusWolf/"
  val corpusZin = "/mnt/DiskStation/homes/jesse/work/Diamant/Data/CorpusZinIndex/"
  val corpusDBNL = "/media/jesse/Data/Diamant/DBNL/"

  def testje(searcher: Searcher) = {
    val struct = searcher.getIndexStructure
    val allAvailableFieldNames = struct.getComplexFields.asScala.toList.map(f => struct.getComplexFieldDesc(f).getProperties.asScala.toList)

    println(allAvailableFieldNames)

    val corpSize = corpusSize(searcher)

    println("corpus Size: " + corpSize)
    val concordancer = Concordancer(searcher)
    val cw = concordancer.concordances("[word='ezel']")
    cw.foreach(println)
    println()
  }

  def testIsThisQuicker() =
  {
    val s = Searcher.open(new java.io.File("/media/jesse/Data/Diamant/CorpusEzel/"))
    val c = Concordancer(s)
    testje(s)
    //c.whyIsItSlowAndHowToSpeedup("[pos='ADP.*']")
  }

  def fcsTest() =
  {
    val s = Searcher.open(new java.io.File("/media/jesse/Data/Diamant/DBNL/"))
    val c = Concordancer(s)
    val h = c.concordances("[lemma='zee']")
    h.foreach(c => println(Concordance.fromXML(c.toXML).tokenProperties.mapValues(x => x.toList) == c.tokenProperties.mapValues(x => x.toList)))
    //h.foreach(c => println(FCS.toXML(c)))
  }

  def bulyTest() =
  {
    val s = Searcher.open(new java.io.File("/media/jesse/Data/Diamant/CorpusBunzingLynx/"))
    val c = Concordancer(s)
    val h = c.concordances("[lemma='bunzing|lynx']") // .filter(x => x("word")(x.hitStart).matches("^[lb].*"))
    Console.err.println("Total size: " + h.size)
    tester.anonTest(h.toList)
  }

  def main(args: Array[String]): Unit = {

    val indexDirectory = if (configuration.Configuration.atHome) corpusDBNL else "/datalokaal/Corpus/BlacklabServerIndices/StatenGeneraal/"
    // testIsThisQuicker()
    bulyTest()
    //fcsTest()
    //val searcher = Searcher.open(new java.io.File(indexDirectory))

    //wsdTestZin(searcher)

  }
}
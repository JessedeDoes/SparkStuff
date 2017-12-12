package corpus

import nl.inl.blacklab.queryParser.corpusql.CorpusQueryLanguageParser
import nl.inl.blacklab.search.grouping._
import nl.inl.blacklab.search.{Hits, Kwic, Searcher}
import nl.inl.util.LuceneUtil
import org.apache.lucene.analysis.Analyzer
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.document.Document
import org.apache.lucene.index.Term
import org.apache.lucene.search.spans.SpanQuery
import org.apache.lucene.search.{Query, QueryWrapperFilter}

import scala.collection.immutable.Map

//import Conc.{corpusDBNL, wsdTestZin}
import concordance.{Collocation, Concordance, Filter}
import corpus.Concordancer.{corpusSize, luceneTermFreq}

import scala.collection.JavaConverters._

case class Concordancer(searcher: Searcher)
{

  def getMetadata(fields: List[HitPropertyDocumentStoredField], i: Int): Map[String, String] =
    fields.map(f => f.getName -> f.get(i).toString()).toMap


  def getFieldValue(doc: Document, s: String): String =
    try
      doc.getField(s).stringValue()
    catch {
      case e: Exception => ""
    }


  def getMetadata(doc: Document, fields: List[String]): Map[String, String] =
    fields.map(s => s -> getFieldValue(doc, s)).toMap


  def concordancesStupid(corpusQlQuery: String): Stream[Concordance] = {
    val hits = Concordancer.filteredSearch(searcher, corpusQlQuery, null)

    println(s"hits created for ${corpusQlQuery}!")
    hits.settings.setContextSize(50)
    hits.settings.setMaxHitsToRetrieve(Int.MaxValue)

    val metaFields = searcher.getIndexStructure.getMetadataFields.asScala.toList.sorted


    val iterator: Iterator[Concordance] =
      for {h <- hits.iterator().asScala; kwic = hits.getKwic(h)}
        yield {
          val meta = getMetadata(searcher.document(h.doc), metaFields)
          createConcordance(kwic, meta)
        }
    iterator.toStream
  }
  val portion = 10

  def whyIsItSlowAndHowToSpeedup(corpusQlQuery: String) =
  {
    val hits = Concordancer.filteredSearch(searcher, corpusQlQuery, null)

    println(s"hits created for ${corpusQlQuery}!")
    hits.settings.setContextSize(6)
    hits.settings.setMaxHitsToRetrieve(Int.MaxValue)
    val hw = hits.window(0,portion)
    val hwi = hw.iterator().asScala
    while (hwi.hasNext) {

      val h = hwi.next()
      //hits.window(0,portion).getK
      val kwic = hw.getKwic(h) // ok dit lijkt de boosdoener te zijn; moet dus anders... beter getKwic op hw ? Jawel; dat werkt
      println(kwic)
    }
  }


  def concordances(corpusQlQuery: String): Stream[Concordance] = {
    val hits = Concordancer.filteredSearch(searcher, corpusQlQuery, null)

    println(s"hits created for ${corpusQlQuery}!")
    hits.settings.setContextSize(10)
    hits.settings.setMaxHitsToRetrieve(Int.MaxValue)

    val metaFields = searcher.getIndexStructure.getMetadataFields.asScala.toList.sorted

    val z = Stream.from(0).map(x => portion * x) // wat gebeurt er als hits op zijn??
    z.flatMap(k => {
      try {
        // Console.out.println(s"at ${k}")

        val hw = hits.window(k, portion);
        val iterator: Iterator[Concordance] =
          for {h <- hw.iterator().asScala; kwic = hw.getKwic(h)}
            yield {
              val meta = getMetadata(searcher.document(h.doc), metaFields)
              createConcordance(kwic, meta)
            }
        iterator.toStream
      } catch { case ex: Exception => List(null).toStream }
    }
    ).takeWhile(_ != null)
  }


  def createConcordance(kwic: Kwic, meta: Map[String, String]): Concordance = {
    val tokenProperties = kwic.getProperties.asScala.toList.map(
      s => s ->
        (kwic.getLeft(s).asScala.toList ++ kwic.getMatch(s).asScala.toList ++ kwic.getRight(s).asScala.toList).toArray).toMap

    val metaKeys = meta.keys.toList.sorted
    val metaValues = metaKeys.map(s => meta(s))
    new Concordance(kwic.getHitStart, kwic.getHitEnd, tokenProperties, meta)
  }

  def collocations(c0: Stream[Concordance]): List[(String, Int, Int, Double)] = {
    val f = Filter("pos", "AA.*")
    val f1 = c0.count(x => true)
    val corpSize = corpusSize(searcher)
    val contextFrequencies = Collocation.contextFrequencies(c0, f)
    val enhanced = contextFrequencies
      .filter({ case (t, f) => f > 0.005 * f1 && t.matches("^[a-z]+$") })
      .map({ case (t, f) => (t, f, luceneTermFreq(searcher, t)) })

    enhanced.map({ case (t, f, f2) => (t, f, f2, Collocation.salience(f, f1, f2, corpSize.asInstanceOf[Int])) })
  }

}

object Concordancer
{
  def parseLuceneQuery(s: String, m: String): Query = {
    val a: Analyzer = new StandardAnalyzer();
    try
      return LuceneUtil.parseLuceneQuery(s, a, m)
    catch {
      case e: Exception =>
        // TODO Auto-generated catch block
        e.printStackTrace
    }
    null
  }
  def singleWordQuery(s: String): String = s"[lemma='${s}']"

  def termFrequency(searcher: Searcher, w: String) = Concordancer.frequency(searcher, singleWordQuery(w), null)

  def luceneTermFreq(searcher: Searcher, w: String) = searcher.getIndexReader.totalTermFreq(new Term("contents%lemma@s", w)).asInstanceOf[Int]

  def corpusSize(searcher: Searcher): Long = {
    //searcher.getIndexSearcher.collectionStatistics(searcher.getIndexStructure.).sumTotalTermFreq
    searcher.getIndexStructure.getTokenCount
  }

  def filteredSearch(searcher: Searcher, corpusQlQuery: String, filterQueryString: String): Hits = {
    val hits = searcher.find(createSpanQuery(searcher, corpusQlQuery, filterQueryString)) // dit is niet optimaal...
    hits
  }

  def createSpanQuery(searcher: Searcher, corpusQlQuery: String, filterQueryString: String): SpanQuery = {
    val pattern = CorpusQueryLanguageParser.parse(corpusQlQuery)

    if (filterQueryString != null && filterQueryString.length > 0) {
      val filterQuery = parseLuceneQuery(filterQueryString, "")
      val filter = new QueryWrapperFilter(filterQuery)
      searcher.createSpanQuery(pattern, filter)
    } else searcher.createSpanQuery(pattern)
  }


  def frequency(searcher: Searcher, corpusQlQuery: String, filterQueryString: String): Int = {
    val q = createSpanQuery(searcher, corpusQlQuery, filterQueryString);
    val hits = filteredSearch(searcher, corpusQlQuery, null)

    println("hits created for: " + corpusQlQuery);
    hits.settings.setContextSize(0);
    hits.settings.setMaxHitsToRetrieve(Int.MaxValue)
    hits.size
  }
}
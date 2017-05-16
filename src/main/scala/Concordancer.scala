import nl.inl.blacklab.search.{Concordance,Kwic,Hit,Hits,Searcher,Span}
import nl.inl.blacklab.search.grouping._
import nl.inl.blacklab.queryParser.corpusql.CorpusQueryLanguageParser

import org.apache.lucene.search.{Query,QueryWrapperFilter}
import org.apache.lucene.document.Document
import org.apache.lucene.search.spans.SpanQuery;

import nl.inl.util.LuceneUtil
import org.apache.lucene.analysis.Analyzer
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.queryparser.classic.ParseException

import scala.collection.immutable.{Map,HashMap}

import org.apache.spark.{SparkConf,SparkContext}

import org.apache.spark.SparkContext._
import org.apache.spark.rdd.JdbcRDD
import org.apache.spark.sql._
import org.apache.spark.sql.types._
import org.apache.spark.sql.functions._

import java.sql.{DriverManager,Connection,ResultSet}

import scala.collection.JavaConverters._
import com.esotericsoftware.minlog._

class Concordancer(s: Searcher) {

	val searcher = s

  def parseLuceneQuery(s: String, m: String): Query = 
  {
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

	@throws[ParseException]
	@throws[nl.inl.blacklab.queryParser.corpusql.ParseException]
	def filteredSearch(searcher: Searcher, corpusQlQuery: String, filterQueryString: String): Hits =
	{
		val hits = searcher.find(createSpanQuery(searcher, corpusQlQuery, filterQueryString)) // dit is niet optimaal...
								hits
	}

	def createSpanQuery(searcher: Searcher, corpusQlQuery: String, filterQueryString: String): SpanQuery = 
	{
	  val pattern = CorpusQueryLanguageParser.parse(corpusQlQuery)
								
		if (filterQueryString != null && filterQueryString.length > 0) 
		{
			val filterQuery = parseLuceneQuery(filterQueryString, "")
			val filter = new QueryWrapperFilter(filterQuery)
			searcher.createSpanQuery(pattern, filter)
		} else searcher.createSpanQuery(pattern)
	}
	


	// type Concordance = List[(String,List[String])]

	def getMetadata(fields:List[HitPropertyDocumentStoredField], i:Int) : Map[String,String] =
	    fields.map(f => f.getName ->  f.get(i).toString()).toMap
	   
	
	def getFieldValue(doc:Document, s:String):String =
	  try
	    doc.getField(s).stringValue()
	  catch
	  {
	    case e:Exception => ""
	  }
	
	
	def getMetadata(doc:Document, fields: List[String]): Map[String,String] =
	  fields.map(s => s -> getFieldValue(doc,s)).toMap
	
	

	def collectConcordances(searcher: Searcher, corpusQlQuery: String, session: SparkSession):  DataFrame = 
		{
	      println()
				val hits = filteredSearch(searcher, corpusQlQuery, null)
						hits.setContextSize(4)
						hits.setMaxHitsToRetrieve(100000)
						println("hits created!");
	      
	      val metaFields = searcher.getIndexStructure.getMetadataFields.asScala.toList.sorted
	      //val StoredFields = metaFields.map(s => new HitPropertyDocumentStoredField(hits,s, s))
			
				val schema = createSchema(hits, metaFields)
				
				val iterator:Iterator[Row] = for { h <- hits.iterator().asScala;   kwic = hits.getKwic(h) }
				yield 
				{
				   val meta = getMetadata(searcher.document(h.doc), metaFields)
				   createRow(kwic, meta)
				}
				createDataFrame(iterator,session,schema)
		}



	def createSchema(hits:Hits, metaFields: List[String]):StructType = 
		{
	    val kwic = hits.getKwic(hits.get(0))
	    //kwic.toConcordance().
		  //println("Document properties:" + kwic.getDocContents.getProperties().asScala.toList)
			val fields = kwic.getProperties().asScala.toList

			val tokenFields = fields.map(fieldName => StructField(fieldName, new ArrayType(StringType,false), nullable = true))
			val metaFieldz = metaFields.map(fieldName => StructField(fieldName, StringType, nullable = true))
			val extraFields = List(
								StructField("hitStart", IntegerType, nullable=false),
								StructField("hitEnd", IntegerType, nullable=false))
			val schema = StructType(extraFields ++ tokenFields ++ metaFieldz)
			println(schema)
			schema
		}

	def createDataFrame(rows: Iterator[Row], spark: SparkSession, schema: StructType):DataFrame= 
		{
				val rowz = spark.sparkContext.parallelize(rows.toList, 1)

						spark.createDataFrame(rowz, schema)
		} 

	def createRow(kwic:Kwic, meta:Map[String,String]): Row = 
		{
				val tokenValues  = kwic.getProperties().asScala.toList.map(
						s => (kwic.getLeft(s).asScala.toList ++ kwic.getMatch(s).asScala.toList 
						    ++ kwic.getRight(s).asScala.toList).toArray);

				val metaKeys = (meta.keys.toList.sorted)
				val metaValues = metaKeys.map(s => meta(s))
				Row.fromSeq(kwic.getHitStart :: kwic.getHitEnd :: tokenValues ++ metaValues)
		} 
}


object Conc
{
	val sparkSession = SparkSession.builder
			    .master("local")
			    .appName("My App")
			    .getOrCreate()

			//val sc0 = new SparkContext(conf)
	val sc = sparkSession.sparkContext
	
	
	def testBlacklabQuery(searcher: Searcher) =
	{
	  	val c = new Concordancer(searcher)
	  	val df = c.collectConcordances(searcher, "[pos='AA.*'][lemma='feit']", sparkSession)
	  	for { conc <- 
				  df.filter("pos[hitStart-1]='ADV()' and pos[hitEnd]='ADV()'").sort(desc("date")).selectExpr("date", "word", "lemma[hitStart] as lemma")   } 
		  {
		      println(conc.getAs[String]("date") + " <"  + conc.getAs[String]("lemma") + "> " + conc.getAs[Array[_]]("word"))
		  }
		  df
	}
	
	def lemmaJoin(concordances:DataFrame, lemmaSet:DataFrame): DataFrame =
	{
	  val joinedDF = concordances.join(lemmaSet, concordances("lemma") === lemmaSet("modern_lemma"), "inner")
	  joinedDF
	}
	
	def main(args: Array[String])
	{
	  //Log.set(Log.LEVEL_ERROR)
	
	  val indexDirectory = if (TestSpark.atHome) "/media/jesse/Data/Diamant/StatenGeneraal/" else "/datalokaal/Corpus/BlacklabServerIndices/StatenGeneraal/"
		val searcher = Searcher.open(new java.io.File(indexDirectory))
				println("searcher open...")
		val concordances = testBlacklabQuery(searcher).selectExpr("date", "word", "lemma[hitStart] as lemma", "pos[hitStart] as pos") 
		val lemmata = TestSpark.lemmataDataFrame(sc).filter("not (lemma_gigpos rlike 'AA')")
		val joined = lemmaJoin(concordances, lemmata)
		for (x <- joined)
		  println(x)
	}
}


/*
* Problemen als je wil paralleliseren: lucene query is niet puur of doet iedere keer nieuwe initialisatie
* 
* Oplossing ?! initialisatie een keer per node zie
* Zie http://stackoverflow.com/questions/40435741/object-cache-on-spark-executors?rq=1 (tweede manier)
* Maar dan wil je de grotere corpora maar op 1 node laden, dus
* zou je expliciet willen mappen welke deelvraag waarheen gaat, wat niet echt in de geest van het hele gespark is (?)  
*/

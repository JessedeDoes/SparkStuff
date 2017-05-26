import nl.inl.blacklab.search.{Kwic,Hit,Hits,Searcher,Span}
import nl.inl.blacklab.search.grouping._
import nl.inl.blacklab.queryParser.corpusql.CorpusQueryLanguageParser

import org.apache.lucene.search.{Query,QueryWrapperFilter}
import org.apache.lucene.document.Document
import org.apache.lucene.search.spans.SpanQuery;

import nl.inl.util.LuceneUtil
import org.apache.lucene.analysis.Analyzer
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.queryparser.classic.ParseException

import org.apache.lucene.search.spans.SpanQuery;



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



class Concordancer {

	//val searcher = s

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
	



  def frequency(searcher: Searcher, corpusQlQuery: String, filterQueryString: String): Int = 
  {
    val q = createSpanQuery(searcher, corpusQlQuery, filterQueryString);
    val hits = filteredSearch(searcher, corpusQlQuery, null)
				
		println("hits created for: " + corpusQlQuery);
	  hits.settings.setContextSize(0); 
	  hits.settings.setMaxHitsToRetrieve(Int.MaxValue) 
    hits.size
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
				val hits = filteredSearch(searcher, corpusQlQuery, null)
				
				println("hits created!");
	      hits.settings.setContextSize(4); 
	      hits.settings.setMaxHitsToRetrieve(100000) 
	      
	      val metaFields = searcher.getIndexStructure.getMetadataFields.asScala.toList.sorted
	      
			
				val schema = createSchema(hits, metaFields)
				
				val iterator:Iterator[Row] = for { h <- hits.iterator().asScala;   kwic = hits.getKwic(h) }
				yield 
				{
				   val meta = getMetadata(searcher.document(h.doc), metaFields)
				   createRow(kwic, meta)
				}
				createDataFrame(iterator,session,schema)
		}
	
		def concordances(searcher: Searcher, corpusQlQuery: String):  Stream[Concordance] = 
		{
				val hits = filteredSearch(searcher, corpusQlQuery, null)
				
				println("hits created!");
	      hits.settings.setContextSize(50); 
	      hits.settings.setMaxHitsToRetrieve(1000000) 
	      
	      val metaFields = searcher.getIndexStructure.getMetadataFields.asScala.toList.sorted
	      
				
				val iterator:Iterator[Concordance] = 
				for { h <- hits.iterator().asScala;   kwic = hits.getKwic(h) }
				yield 
				{
				   val meta = getMetadata(searcher.document(h.doc), metaFields)
				   createConcordance(kwic, meta)
				}
				iterator.toStream
		}

			

	def createSchema(hits:Hits, metaFields: List[String]):StructType = 
		{
	    val kwic = hits.getKwic(hits.get(0))
	   
			val fields = kwic.getProperties().asScala.toList
			
			val tokenFields = fields.map(StructField(_, new ArrayType(StringType,false), nullable = true))
			val metaFieldz = metaFields.map(StructField(_, StringType, nullable = true))
			val extraFields = List("hitStart","hitEnd").map(StructField(_, IntegerType, nullable=false))
/*
			val tokenFields = fields.map(fieldName => StructField(fieldName, new ArrayType(StringType,false), nullable = true))
			val metaFieldz = metaFields.map(fieldName => StructField(fieldName, StringType, nullable = true))
			val extraFields = List(
								StructField("hitStart", IntegerType, nullable=false),
								StructField("hitEnd", IntegerType, nullable=false))
		
			
			*/
			val schema = StructType(extraFields ++ tokenFields ++ metaFieldz)
			println("Schema:" + schema)
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
	def createConcordance(kwic:Kwic, meta:Map[String,String]): Concordance = 
		{
				val tokenProperties  = kwic.getProperties().asScala.toList.map(
						s => (s -> 
						(kwic.getLeft(s).asScala.toList ++ kwic.getMatch(s).asScala.toList ++ kwic.getRight(s).asScala.toList).toArray)).toMap;

				val metaKeys = (meta.keys.toList.sorted)
				val metaValues = metaKeys.map(s => meta(s))
				new Concordance(kwic.getHitStart, kwic.getHitEnd, tokenProperties, meta)
		} 
	
	
}


object Conc
{
	lazy val sparkSession = SparkSession.builder
			    .master("local")
			    .appName("My App")
			    .getOrCreate()

			//val sc0 = new SparkContext(conf)
	lazy val sc = sparkSession.sparkContext
	
	
	def testBlacklabQuery(searcher: Searcher) =
	{
	  	val c = new Concordancer
	  	val df = c.collectConcordances(searcher, "[pos='AA.*'][lemma='gezindheid']", sparkSession)
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
	
	def testjeMetSpark(args: Array[String]) = 
	{
	  //Log.set(Log.LEVEL_ERROR)
	
	  val indexDirectory = if (TestSpark.atHome) "/media/jesse/Data/Diamant/CorpusWolf/" else "/datalokaal/Corpus/BlacklabServerIndices/StatenGeneraal/"
		val searcher = Searcher.open(new java.io.File(indexDirectory))
				println("searcher open...")
		val concordances = testBlacklabQuery(searcher).selectExpr("date", "word", "lemma[hitStart] as lemma", "pos[hitStart] as pos") 
		val lemmata = TestSpark.lemmataDataFrame(sc).filter("not (lemma_gigpos rlike 'AA')")
		val joined = lemmaJoin(concordances, lemmata)
		for (x <- joined)
		  println(x)
	}
	
	def singleWordQuery(s:String):String = s"[lemma='${s}']"
	def termFrequency(searcher:Searcher, w:String) = (new Concordancer).frequency(searcher, singleWordQuery(w), null)
	
	def corpusSize(searcher:Searcher):Long = 
	{
	  //searcher.getIndexSearcher.collectionStatistics(searcher.getIndexStructure.).sumTotalTermFreq
	  searcher.getIndexStructure.getTokenCount
	}
	
	def main(args: Array[String]):Unit = 
  {
     val indexDirectory = if (TestSpark.atHome) "/media/jesse/Data/Diamant/CorpusWolf/" else "/datalokaal/Corpus/BlacklabServerIndices/StatenGeneraal/"
		 val searcher = Searcher.open(new java.io.File(indexDirectory))
		 val concordancer = new Concordancer
    
     // searcher.termFrequencies(documentFilterQuery, fieldName, propName, altName)
     
     // val fl = List("paard","varken","koe","wolf","bunzing","hond", "vlieg").map(s => singleWordQuery(s)).map(q => (q,concordancer.frequency(searcher, q, null)))
     
     // println(fl)
		 
     val corpSize =  corpusSize(searcher)
     
     println("corpus Size: " + corpSize)
     
     val q0 = "[lemma='schuur' & word='(?c)schu.*' & pos='NOU-C.*']"
     val c0 = concordancer.concordances(searcher, q0)
     val f1 = c0.count(( x => true))
     println(s"Hits for ${q0} : ${f1}")  
     val f = Filter("pos",".*")
  
     
     lazy val contextFrequencies = Collocation.contextFrequencies(c0,f)
    
     
     val enhanced = contextFrequencies
              .filter( {case (t,f) => f > 0.05 * f1 && t.matches("^[a-z]+$") } )
              .map( { case (t,f) => (t,f,termFrequency(searcher,t)) })
     
     val scored = enhanced.map( { case (t,f,f2) => (t,f,f2,Collocation.dice(f, f1, f2, corpSize))} )
    
     for (s <- scored.sortWith({ case (a,b) => a._4 < b._4 } ))
      println(s)
     println()
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

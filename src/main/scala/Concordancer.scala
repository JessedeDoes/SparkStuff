import nl.inl.blacklab.search.{Concordance,Kwic,Hit,Hits,Searcher,Span}
import nl.inl.blacklab.queryParser.corpusql.CorpusQueryLanguageParser

import org.apache.lucene.search.{Query,QueryWrapperFilter}

import org.apache.lucene.search.spans.SpanQuery;

import nl.inl.util.LuceneUtil
import org.apache.lucene.analysis.Analyzer
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.queryparser.classic.ParseException



import org.apache.spark.{SparkConf,SparkContext}

import org.apache.spark.SparkContext._
import org.apache.spark.rdd.JdbcRDD
import org.apache.spark.sql._
import org.apache.spark.sql.types._

import java.sql.{DriverManager,Connection,ResultSet}

import scala.collection.JavaConverters._

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
						
								// Execute the TextPattern
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

	def collectConcordances(searcher: Searcher, corpusQlQuery: String, session: SparkSession):  DataFrame = 
		{
				val hits = filteredSearch(searcher, corpusQlQuery, null)
						hits.setContextSize(2)
						hits.setMaxHitsToRetrieve(100000)
						println("hits created!");

			
				val schema = createSchema(hits)

				val i = for { h <- hits.iterator().asScala; conc = hits.getKwic(h) }
				yield  createRow(conc)

				createDataFrame(i,session,schema)

		}



	def createSchema(hits:Hits):StructType = 
		{
	    val kwic = hits.getKwic(hits.get(0))
		  
			val fields = kwic.getProperties().asScala.toList

			val fieldDefs = fields.map(fieldName => StructField(fieldName, new ArrayType(StringType,false), nullable = true))
			val extraFields = List(
								StructField("hitStart", IntegerType, nullable=false),
								StructField("hitEnd", IntegerType, nullable=false))
			val schema = StructType(extraFields ++ fieldDefs)
			println(schema)
			schema
		}

	def createDataFrame(rows: Iterator[Row], spark: SparkSession, schema: StructType):DataFrame= 
		{
				val rowz = spark.sparkContext.parallelize(rows.toList, 1)

						spark.createDataFrame(rowz, schema)
		} 

	def createRow(kwic:Kwic): Row = 
		{
				val c  = kwic.getProperties().asScala.toList.map(
						s => (kwic.getLeft(s).asScala.toList ++ kwic.getMatch(s).asScala.toList ++ kwic.getRight(s).asScala.toList).toArray);

				
				Row.fromSeq(kwic.getHitStart :: kwic.getHitEnd :: c)
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
	
	
	def main(args: Array[String])
	{
		val searcher = Searcher.open(new java.io.File("/media/jesse/Data/Diamant/StatenGeneraal/"))
				println("searcher open...")
				val c = new Concordancer(searcher)
				for { conc <- c.collectConcordances(searcher, "[pos='AA.*'][lemma='eigenaardigheid']", sparkSession)
				  .filter("pos[hitStart-1]='ADV()'") } println(conc)
	}
}
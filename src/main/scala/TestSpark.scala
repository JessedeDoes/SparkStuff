
import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import java.sql.DriverManager
import java.sql.Connection
import java.sql.ResultSet
import org.apache.spark.rdd.JdbcRDD
import org.apache.spark.sql._
import org.apache.spark.sql.types._

//import org.apache.spark.sql.SQLContext
//http://digitopia.nl/workshop/cheatsheet.html

object TestSpark 
{
	//val conf = new SparkConf().setMaster("local").setAppName("My App")
	val sparkSession = SparkSession.builder.
      master("local")
      .appName("My App")
      .getOrCreate()
      
	//val sc0 = new SparkContext(conf)
 	val sc = sparkSession.sparkContext
 	


	def createConnection() = {
					Class.forName("org.postgresql.Driver").newInstance();
					DriverManager.getConnection("jdbc:postgresql://localhost/gig_pro_dev?user=postgres&password=inl");
	}
	def extractValues(r: ResultSet) = {
			(r.getString(2), r.getInt(1), r.getString(3))
	}

	def loadSome(sc: SparkContext):JdbcRDD[(String,Int, String)] = 
		{
				val data = new JdbcRDD(sc,
						createConnection, 
						"""SELECT lemma_id, modern_lemma, lemma_gigpos 
						FROM data.lemmata 
						where lemma_id > ? and lemma_id < ? and parent_id is 
						null order by parent_id""",
						lowerBound=0, upperBound=10000000, numPartitions = 1, mapRow = extractValues)
						data
		}

	def lemmataDataFrame(sc: SparkContext):DataFrame = 
		{
			dataFrameFromQuery(sc, "(select lemma_id, modern_lemma, lemma_gigpos from data.lemmata) as lemz" )
		}
	
	def flectableAA(sc: SparkContext):DataFrame = dataFrameFromQuery(sc,flectable_query)
	
	val flectable_query = """
	  (select distinct lemma_id, modern_lemma, lemma_gigpos from data.lemmata_en_paradigma_view where
	  wordform_gigpos ~ 'AA.*infl=e' and wordform ~ 'e$'  and not (modern_lemma ~ 'e$')) as flectable" 
	   """ 
              
	def dataFrameFromQuery(sc: SparkContext, query: String):DataFrame = 
		{
				val sqlContext = new org.apache.spark.sql.SQLContext(sc)
				import sqlContext.implicits._
				val url = "jdbc:postgresql://localhost/gig_pro_dev?user=postgres&password=inl"
				val df = sqlContext.load("jdbc", Map(
								"url" -> url,
								"dbtable" -> query))
				df
		}
	
	def makeDataframe(sc: SparkSession):DataFrame = lemmataDataFrame(sc.sparkContext)
		
	def wordCount(inputFile: String) =
	{
	  val input = sc.textFile(inputFile)
		val words = input.flatMap(line => line.split(" "))
		val counts = words.map(word => (word, 1)).reduceByKey{case (x, y) => x + y}		
		println(counts)
	}
	
	def main(args: Array[String]) =
		{
				// Load our input data.
				
				
				val z = lemmataDataFrame(sc).filter("modern_lemma='mooi'");
			
				println(z.columns.toList)
				println(z.collect().toList) 
		}
}
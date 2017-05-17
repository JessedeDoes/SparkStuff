import wsd._
import scala.collection.JavaConverters._
import org.apache.spark.{SparkConf,SparkContext}

import org.apache.spark.SparkContext._
import org.apache.spark.rdd.JdbcRDD
import org.apache.spark.sql._
import org.apache.spark.sql.types._
import org.apache.spark.sql.functions._

object ConvertOldInstanceBase 
{
   val sparkSession:SparkSession = SparkSession.builder
			    .master("local")
			    .appName("My App")
			    .getOrCreate()

			//val sc0 = new SparkContext(conf)
	 val sc = sparkSession.sparkContext
   val tokenFields = List("word", "lemma" , "pos")
   
   def makeSchema(fields:List[String]):StructType = 
   {
     	val tokenFields = fields.map(fieldName => StructField(fieldName, new ArrayType(StringType,false), nullable = true))
		
			val extraFields = List(
								StructField("hitStart", IntegerType, nullable=false),
								StructField("hitEnd", IntegerType, nullable=false))
			val metaFields = List(
			   StructField("sensId", StringType, nullable=false),
			    StructField("lempos", StringType, nullable=false))
			val schema = StructType(extraFields ++ (tokenFields ++ metaFields))
			schema
   }
   
   val schema = makeSchema(tokenFields)
   
   def convert(fileName:String, session: SparkSession):DataFrame =
   {
     val w:WSDInstanceBase = WSDInstanceBase.loadFromFile(fileName)
     val rows = 
       for (e <- w.entrySet().asScala;
          i <- e.getValue.asScala.map(x => convertInstance(x,e.getKey)))
            yield i
     	session.createDataFrame(session.sparkContext.parallelize(rows.toList, 1), schema)
   }
   
   def convertInstance(i:WSDInstance, groupId:String):Row =
   {
     val t = i.tokens.asScala
     
     val lemmata = t.map(_ getLemma).toArray
     val words = t.map(_ getWord).toArray
     val pos = t.map(_ getPoS).toArray
     
     val tokenFieldMap = List((lemmata,"lemma"), (words,"word"), (pos,"pos") ).map({ case (x,y) => y->x }).toMap
     val tokenValues = tokenFields.map(x => tokenFieldMap(x))
     Row.fromSeq(i.targetPosition :: i.targetPosition+1 :: tokenValues ++List(i.senseId, groupId))
   }
   
   def main(args: Array[String]) = 
   {
     val sparkSession:SparkSession = SparkSession.builder
			    .master("local")
			    .appName("My App")
			    .getOrCreate()
     val frames = convert(args(0), sparkSession)
     frames.write.format("parquet").save("Data/wsdInstanceBase.parquet")
     //frames.rdd.saveAsTextFile("Data/aapje.framez")
   }
}
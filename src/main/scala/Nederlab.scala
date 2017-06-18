import nederlab._
import scala.collection.JavaConverters._
import org.apache.spark.{SparkConf,SparkContext}

import org.apache.spark.SparkContext._
import org.apache.spark.rdd.JdbcRDD
import org.apache.spark.sql._
import org.apache.spark.sql.types._
import org.apache.spark.sql.functions._
object Nederlab
{
  def concordances(cql: String):Stream[Concordance] =
  {
    val schema = null
    val client = new NederlabClient

		client.getHits(cql).iterator().asScala.toStream.map(createConcordance)
  }
  

  def schemaHas(schema:StructType, f:String) =
    schema match 
    { 
       case StructType(l) => l.exists( { case StructField(f, _, _, _) => true; case _ => false } 
    )}

	def createConcordance(hit: Hit): Concordance =
	{
		val tokenFields = hit.knownPrefixes.asScala.toList
		val tokenValues  =
			tokenFields.map(
				s => (
					(s -> hit.tokens.asScala.flatMap(
						(t:Token) => t.tokenProperties.asScala.filter(_.prefix==s).map(_.value)
					).toArray))).toMap
		val mapped = tokenValues - "t" + ("word" -> tokenValues("t"))
		Concordance(hit.startPosition, hit.endPosition, mapped, hit.document.getMetadata.asScala.toMap)
	}


  
  def main(args:Array[String])
  {
    concordances(  """[t_lc="ezel" | tc_lc="ezels"]""" ).foreach(println)
  }
}
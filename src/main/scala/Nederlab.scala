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
  def concordances(cql: String) =
  {
    val client = new NederlabClient
    for (h <- client.getHits(cql).iterator().asScala)
      
      
  }
  
  def createRow(kwic:Hit, meta:Map[String,String]): Row = 
		{
				val tokenValues  = kwic.properties.asScala.toList.map(
						s => (kwic.tokens.map((t:Token) => t.tokenProperties.asScala.filter());

				val metaKeys = (meta.keys.toList.sorted)
				val metaValues = metaKeys.map(s => meta(s))
				Row.fromSeq(kwic.getHitStart :: kwic.getHitEnd :: tokenValues ++ metaValues)
		} 
}
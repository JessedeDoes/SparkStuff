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
    val schema = null
    val client = new NederlabClient
    
    for (h <- client.getHits(cql).iterator().asScala)
      println( createRow(h,null)._1 )
  }
  
  def createSchema(fields:List[String], metaFields: List[String]):StructType = 
		{
			val tokenFields = fields.map(StructField(_, new ArrayType(StringType,false), nullable = true))
			val metaFieldz = metaFields.map(StructField(_, StringType, nullable = true))
			val extraFields = List("hitStart","hitEnd").map(StructField(_, IntegerType, nullable=false))
			val schema = StructType(extraFields ++ tokenFields ++ metaFieldz)
			println("Schema:" + schema)
			schema
		}
  
  def schemaHas(schema:StructType, f:String) =
    schema match 
    { 
       case StructType(l) => l.exists( { case StructField(f, _, _, _) => true; case _ => false } 
    )}
  
  def createRow(hit:Hit, schema:StructType): (Row,StructType) = 
		{
        val tokenFields = hit.knownPrefixes.asScala.toList
				val tokenValues  = 
				  tokenFields.map( 
						s => (
						       hit.tokens.asScala.flatMap( 
						           (t:Token) => t.tokenProperties.asScala.filter(_.prefix==s).map(_.value)
						    )))
				
				val meta = hit.document.getMetadata.asScala		    
				val metaKeys = (meta.keys.toList.sorted)
				if (schema != null && tokenFields.map(s => schemaHas(schema,s)).contains(false))
				{
				  println("HHHHHHHHHHHHHHHHOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOELP")
				  System.exit(1)
				}
				val metaValues = metaKeys.map(s => meta(s))
				(
				    Row.fromSeq(hit.startPosition :: hit.endPosition :: tokenValues ++ metaValues),
				    if (schema==null) createSchema(tokenFields,metaKeys) else schema
				)
		} 
  
  def main(args:Array[String])
  {
    concordances(  """[t_lc="deliberatie" | tc_lc="deliberaties"]""" )
  }
}
/**
  * Created by jesse on 6/18/17.
  */

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import java.sql.DriverManager
import java.sql.Connection
import java.sql.ResultSet
import org.apache.spark.rdd.JdbcRDD
import org.apache.spark.sql._
import org.apache.spark.sql.types._
import collection.JavaConverters._

import concordance.Concordance

object  ConcordanceDataFrame
{
  def createSchema(fields:List[String], metaFields: List[String]):StructType =
  {
    val tokenFields = fields.map(StructField(_, new ArrayType(StringType,false), nullable = true))
    val metaFieldz = metaFields.map(StructField(_, StringType, nullable = true))
    val extraFields = List("hitStart","hitEnd").map(StructField(_, IntegerType, nullable=false))
    val schema = StructType(extraFields ++ tokenFields ++ metaFieldz)
    println("Schema:" + schema)
    schema
  }

  def createSchema(c: Concordance):StructType =
  {
    createSchema(c.tokenProperties.keySet.toList.sorted, c.metadata.keySet.toList.sorted)
  }

  def createRow(c: Concordance):Row =
  {
    val tp = c.tokenProperties.keySet.toList.sorted
    val mp = c.metadata.keySet.toList.sorted
    println(s"Converting: $c")
    println(c("word"))
    val r = Row.fromSeq(c.hitStart :: c.hitEnd ::  tp.map(s => c(s)) ++ mp.map(s => c.meta(s)))
    println(s"${r(8).getClass} : ${r(8)}")
    r
  }

  def createDataFrame(rows: Iterator[Row], spark: SparkSession, schema: StructType):DataFrame=
  {
    val rowz = spark.sparkContext.parallelize(rows.toList, 1)
    rowz.foreach(println)
    println(s"rows created: ${rowz.count()}!!!")
    spark.createDataFrame(rowz, schema)
  }

  def collectConcordances(concordances: Stream[Concordance], session: SparkSession):  DataFrame =
  {
    println(concordances.head)
    val schema = createSchema(concordances.head)

    val iterator:Iterator[Row] = concordances.map(createRow).iterator
    ConcordanceDataFrame.createDataFrame(iterator, session, schema)
  }
}

package database

import java.sql.ResultSet

import hilex.Hilex
import org.skife.jdbi.v2.tweak.ResultSetMapper
import org.skife.jdbi.v2.{Handle, Query, StatementContext}
import scala.collection.JavaConverters._

object DatabaseUtilities
{
  type AlmostQuery[T] = (Handle => Query[T])

  case class GetResult[T](f: ResultSet => T) extends ResultSetMapper[T]
  {
    override def map(arg0: Int, r: ResultSet, arg2: StatementContext): T =
    {
      val w  = f(r)
      w
    }
  }

  case class Select[T](mapping: Diamond => T, from: String)
  {

  }
  case class Voedsel(beest:String, voedsel:Int)

  trait Diamond
  {
    def getString(s:String):String
    def getInt(s:String):Int
  }

  case class Mocky1(resultSet:ResultSet) extends Diamond
  {
    def getString(s:String):String = resultSet.getString(s)
    def getInt(s:String):Int = resultSet.getInt(s)
  }

  class Mocky2 extends Diamond
  {
    val fieldNames: scala.collection.mutable.ListBuffer[String] = new scala.collection.mutable.ListBuffer[String]()
    def getString(s:String):String = { fieldNames.append(s); "wereldvrede"}
    def getInt(s:String):Int = {fieldNames.append(s); 42}
  }

  def slurp[A](db: Handle, a: AlmostQuery[A]):List[A] =
  {
    a(db).list().asScala.toList
  }

  def stream[A](db: Handle, a: AlmostQuery[A]):Stream[A] =
  {
    a(db).iterator().asScala.toStream
  }

  implicit def doeHet[T](s:Select[T]): AlmostQuery[T] =
  {
    val m = new Mocky2
    s.mapping(m)
    val gr = GetResult[T](r => s.mapping(Mocky1(r)))
    val query = "select " + m.fieldNames.mkString(", ") + " from " + s.from
    db => db.createQuery(query).map(gr)
  }

  def main(args:Array[String]):Unit =
  {
    case class Woordje(lemma:String, pos:String, id:String)
    val exampleQuery =
      Select(
        mapping = r =>
          Woordje(r.getString("modern_lemma"), r.getString("lemma_part_of_speech"), r.getString("persistent_id")),
        from = "data.lemmata where lemma_part_of_speech ~ 'NOU'")


    Hilex.stream(exampleQuery).filter(w => w.lemma.length > 3 && w.lemma.reverse == w.lemma).foreach(println)
  }
}

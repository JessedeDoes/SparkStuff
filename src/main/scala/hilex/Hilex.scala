package hilex

import java.util.concurrent.Executors

import concordance.Concordance
import org.postgresql.ds.PGPoolingDataSource
import org.skife.jdbi.v2.{DBI, Handle}
import wsd.ConvertOldInstanceBase

import scala.concurrent.ExecutionContext
import database._
import database.DatabaseUtilities._
import spark.TestSpark

object Hilex {

  case class Configuration(name: String, server: String, database: String, user: String, password: String)

  val hilexAtHome = Configuration(
    name = "gigant_hilex",
    server = "svowdb02",
    database = "gigant_hilex_dev",
    user = "postgres",
    password = "inl")

  val hilexAtWork = Configuration(
    name = "gigant_hilex",
    server = "svprre02",
    database = "diamant_prototype",
    user = "postgres",
    password = "inl")

  val diamantAtHome = hilexAtHome.copy(name = "diamantRow", database = "diamant_vuilnisbak")

  implicit lazy val ec = new ExecutionContext {
    val threadPool = Executors.newFixedThreadPool(5);

    def execute(runnable: Runnable) {
      threadPool.submit(runnable)
    }

    def reportFailure(t: Throwable) {}
  }

  val dev = true

  def makeHandle(conf: Configuration): Handle = {
    val source = new PGPoolingDataSource

    source.setDataSourceName(conf.name)
    source.setServerName(conf.server)
    source.setDatabaseName(conf.database)
    source.setUser(conf.user)
    source.setPassword(conf.password)
    source.setMaxConnections(10)
    val dbi = new DBI(source)
    val gigmolHandle: Handle = dbi.open
    gigmolHandle
  }


  lazy val diamantRuwDB: Handle = makeHandle(diamantAtHome)


  implicit lazy val hilexDB: Handle = if (TestSpark.atHome) makeHandle(hilexAtHome) else makeHandle(hilexAtWork)


  def stream[A](a: AlmostQuery[A]): Stream[A] = DatabaseUtilities.stream(hilexDB, a)


  def slurp[A](a: AlmostQuery[A]): List[A] = DatabaseUtilities.slurp(hilexDB, a)

  import java.io._

  def pickleTo(l: List[Concordance], fileName: String): Unit = {

    val pw = new PrintWriter(new File(fileName))
    val z = l.toArray.pickle.toString
    pw.write(z)
    pw.close
  }

  def findSomeLemmata: List[Lemma] = {
    slurp(hilexQueries.lemmaQueryWhere("modern_lemma ~ '^zin$'"))
  }

  def main(args: Array[String]): Unit = {
    val myLemma = "M016273" // ezel
    val l = slurp(hilexQueries.lemmaQueryWhere(s"persistent_id='${myLemma}'"))
    val zin = l.head
    println(zin)
    val senses = zin.senses
    val romans = senses.filter(s => s.parent_sense_id == null)
    romans.foreach(println)

    val attestationsAsConcordances = romans.flatMap(
      s => hilexQueries.getAttestationsBelow(s)
        .map(s => s.toConcordance)
        .map(c => c.copy(metadata = c.metadata ++ List("senseId" -> s.persistent_id, "lempos" -> "zin:n", ("id", ConvertOldInstanceBase.uuid)))
          .tag(babTagger))
    ).filter(_.hitStart > -1)

    attestationsAsConcordances.foreach(c => println(c.vertical))
  }
}

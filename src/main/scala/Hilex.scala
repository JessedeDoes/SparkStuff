
// http://slick.lightbend.com/doc/3.0.0/sql.html
// http://queirozf.com/entries/slick-3-reference-and-examples
// https://github.com/slick/slick/issues/1161 voor samenstellen queries

//import slick.driver.H2Driver.api._
//import slick.driver.JdbcDriver.api._

import org.postgresql.ds.PGPoolingDataSource

import org.skife.jdbi._
import v2.DBI
import v2.Handle
import v2.Query
import v2.StatementContext
import v2.tweak.ResultSetMapper
import java.sql.ResultSet
import java.sql.SQLException


import java.util.concurrent.Executors
import scala.concurrent._
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.forkjoin._


import scala.util.{Try, Success, Failure}
import scala.reflect._
import scala.pickling.Defaults._
import scala.pickling.json._

import scala.collection.JavaConverters._

import Hilex.hilexDB

case class Lemma(modern_lemma: String, lemma_id:Int, persistent_id:String, pos:String) 
{
   lazy val wordforms = Hilex.slurp(queries.getWordforms(List(this)))
   lazy val senses = Hilex.slurp(queries.getSenses(List(this)))
}

case class SynonymDefinition(sense_id: String, synonym: String)

case class Wordform(lemma: Lemma, analyzed_wordform_id:Int, wordform: String)

case class Attestation(wordform: Wordform, quote:String, start_pos: Int, end_pos: Int, eg_id: String, document:DocumentMetadata)
{
   lazy val senses = ???
   import TokenizerWithOffsets._
   import entities._
   
   
   def sillyCriterium (x:TokenWithOffsets):Int = 
   {
       x match { case TokenWithOffsets(t,s,e) => Math.abs(s - start_pos + e - end_pos) } 
   }
   
   def toConcordance =
   {
     val tokens = TokenizerWithOffsets.tokenize(quote)(really=false)
     val probableHit = tokens.toList.zipWithIndex.minBy(x => sillyCriterium(x._1))

     val tokenStream = tokens.toStream
     
     val c = new Concordance(probableHit._2, probableHit._2+1, List(("word",tokens.map( t =>  entities.substitute(t.token.token) ))).toMap, document.properties)
     c
   }
}

case class DocumentMetadata(persistent_id: String, properties:Map[String,String])

case class Sense(lemma: Lemma, persistent_id: String, lemma_id:String, parent_sense_id: String, definition: String)
{
  lazy val attestations = Hilex.slurp(queries.getAttestationsForSense(List(this)))
  lazy val parentSense = Hilex.slurp(queries.getParentSenses(List(this)))
  lazy val subSenses = Hilex.slurp(queries.getSubSenses(List(this)))
  lazy val synonymDefinitions = queries.getSynonymDefinitions(this)
}

private object util
{
  
}

object queries
{  
     case class GetResult[T](f: ResultSet => T) extends ResultSetMapper[T]
     {
     	 override def map(arg0: Int, r: ResultSet, arg2: StatementContext): T =
     	 {
     	    val w  = f(r)
     	     w
     	  }
     }
     
    val getLemma = GetResult[Lemma](r => Lemma(r.getString("modern_lemma"), r.getInt("lemma_id"),  r.getString("persistent_id"), r.getString("lemma_part_of_speech")))
    type AlmostQuery[T] = (Handle => Query[T])
    
    import util._
    val pos = "ADP"
    
    val dataSchema = if (TestSpark.atHome) "data" else "gigant_hilex_data"
    val senseSchema = "wnt_ids"
    
    def lemmaQueryExample():AlmostQuery[Lemma] = 
    {
      db => db.createQuery(s"""
      select modern_lemma, lemma_id,persistent_id, lemma_part_of_speech from data.lemmata 
        where lemma_part_of_speech ~ '${pos}'""").map(getLemma)
    }
    
    def lemmaQueryWhere(where:String):AlmostQuery[Lemma] = 
    {
      
      db => db.createQuery(s"""
      select modern_lemma, lemma_id,persistent_id, lemma_part_of_speech from ${dataSchema}.lemmata 
        where ${where}""").map(getLemma)
    }
    
    def intValues(l:List[Int]) = "(" + l.mkString(",") + ")"
    def stringValues(l:List[String]) = "('" + l.mkString("','") + "')"
    
    def getWordforms(lemmata: List[Lemma]):AlmostQuery[Wordform]  = 
    {
      val ids = lemmata.map(_.lemma_id)
      val lemmaMap = lemmata.map(l => (l.lemma_id,l)).toMap
      implicit val makeWordform = GetResult[Wordform](
          r => Wordform(lemmaMap(r.getInt("lemma_id")), r.getInt("analyzed_wordform_id"), r.getString("wordform"))
      )
      val q = s"""
        select 
                       lemma_id, analyzed_wordform_id, wordform 
        from 
            ${dataSchema}.analyzed_wordforms a, 
            ${dataSchema}.wordforms w 
        where a.wordform_id=w.wordform_id and lemma_id in """ + intValues(ids)
        
        db => db.createQuery(q).map(makeWordform)
    }
     
     def getSenses(lemmata: List[Lemma]):AlmostQuery[Sense] = 
     {
      val ids = lemmata.map(_.persistent_id)
      val lemmaMap = lemmata.map(l => (l.persistent_id,l)).toMap
      println(lemmaMap.toList)
      implicit val makeSense = GetResult[Sense](
          r => Sense(lemmaMap(r.getString("lemma_id")), r.getString("persistent_id"), r.getString("lemma_id"), r.getString("parent_sense_id"), r.getString("definition"))
      )
      val q = s"""
          select lemma_id, persistent_id, lemma_id, parent_sense_id , definition
           from ${senseSchema}.senses 
           where lemma_id in """ + stringValues(ids)
       db => db.createQuery(q).map(makeSense)
     }
     
     def getSubSenses(senses: List[Sense]):AlmostQuery[Sense] = 
     {
      val ids = senses.map(_.persistent_id)
    
      implicit val makeSense = GetResult[Sense](
          r => Sense(
              getLemmaByPersistentId(r.getString("lemma_id")), 
              r.getString("persistent_id"), 
              r.getString("lemma_id"), 
              r.getString("parent_sense_id"), 
              r.getString("definition"))
      )
      
     
      val q = s"""
          select lemma_id, persistent_id, lemma_id, parent_sense_id , definition
           from ${senseSchema}.senses 
           where parent_sense_id in """ + stringValues(ids)
           
       db => db.createQuery(q).map(makeSense) 
     }
    
     def getParentSenses(senses: List[Sense]):AlmostQuery[Sense] = 
     {
      val ids = senses.map(_.parent_sense_id)
   
      implicit val makeSense = GetResult[Sense](
          r => Sense(
              getLemmaByPersistentId(r.getString("lemma_id")), 
              r.getString("persistent_id"), 
              r.getString("lemma_id"), 
              r.getString("parent_sense_id"), 
              r.getString("definition"))
      )
      val q =  s"""
          select lemma_id, persistent_id, lemma_id, parent_sense_id , definition
           from ${senseSchema}.senses 
           where persistent_id in """ +  stringValues(ids)
        db => db.createQuery(q).map(makeSense)
     }
     
     def getAttestationsBelow(s: Sense):List[Attestation] =
       s.attestations ++  s.subSenses.flatMap(s => getAttestationsBelow(s))
     
      def getDocument(r:ResultSet):DocumentMetadata =
      {
         val items =
          List(
              ("author", r.getString("author")),
              ("title", r.getString("title")),
              ("year_from", r.getInt("year_from").toString),
              ("year_to", r.getInt("year_to").toString),
              ("dictionary", r.getString("dictionary"))
           ).toMap
           DocumentMetadata("bla", items)
      }
      
      def getAttestation(r:ResultSet):Attestation = 
      {
         val a =  Attestation(getWordform(r.getInt("analyzed_wordform_id")), 
              r.getString("quote"),  
              r.getInt("start_pos"), 
              r.getInt("end_pos"), r.getString("eg_id"),getDocument(r))
         a.copy(document=a.document.copy(persistent_id=a.eg_id))
      }
      
    def getAttestations(wordforms: List[Wordform]):AlmostQuery[Attestation] =
    {
      val ids = wordforms.map(_.analyzed_wordform_id)
      val wordformMap = wordforms.map(l => (l.analyzed_wordform_id,l)).toMap
    
      implicit val makeAttestation =   GetResult[Attestation](
            r => getAttestation(r))
  
      val q = s"""
      select 
           a.analyzed_wordform_id,
           quote, 
           start_pos,
           end_pos, 
           eg_id,
           d.author,
           d.title,
           d.year_from,
           d.year_to,
           d.dictionary
      from
           #${dataSchema}.analyzed_wordforms a, #${dataSchema}.token_attestations t, wnt_ids.documents d
      where
             d.document_id=t.document_id
            and a.analyzed_wordform_id=t.analyzed_wordform_id 
            and t.analyzed_wordform_id in """ +  intValues(ids)
       db => db.createQuery(q).map(makeAttestation)
    } 
    
    def getLemma(lemma_id: Int):Lemma =
    {
        val l = Hilex.slurp(queries.lemmaQueryWhere(s"lemma_id='${lemma_id}'" ))
        if (l.isEmpty)
         null
       else
         l.head
    }
    
    def getLemmaByPersistentId(lemma_id: String):Lemma =
    {
        val l = Hilex.slurp(queries.lemmaQueryWhere(s"persistent_id='${lemma_id}'" ))
        if (l.isEmpty)
         null
       else
         l.head
    }
    
    implicit def getWordform(analyzed_wordform_id: Int):Wordform = 
    {
      implicit val makeWordform = GetResult[Wordform](
          r => Wordform(getLemma(r.getInt("lemma_id")), r.getInt("analyzed_wordform_id"), r.getString("wordform"))
      )
      val q = s"""
        select 
          lemma_id, analyzed_wordform_id, wordform 
        from 
            ${dataSchema}.analyzed_wordforms a, ${dataSchema}.wordforms w 
        where 
        a.wordform_id=w.wordform_id and analyzed_wordform_id=${analyzed_wordform_id} """
       
       val a =  (db:Handle) => db.createQuery(q).map(makeWordform) 
      
       val l:List[Wordform] = Hilex.slurp(a)
       if (l.isEmpty)
         null
       else
         l.head
    }
    
    def getSynonymDefinitions(sense: Sense):List[SynonymDefinition] =
    {
      
       implicit val makeSynonymDefinition = GetResult[SynonymDefinition](r => SynonymDefinition(r.getString("persistent_id"), r.getString("syn")))
       val q = s""" 
         select
            persistent_id, syn
         from
           wnt.modsyn
          where
           sense_id=${sense.persistent_id}
        """
       val a =  (db:Handle) => db.createQuery(q).map(makeSynonymDefinition)
       val l:List[SynonymDefinition] = Hilex.slurp(a,Hilex.diamantRuwDB)
       l
    }
    
    def getAttestationsForSense(senses: List[Sense]):AlmostQuery[Attestation] =
    {
      val ids = senses.map(_.persistent_id)
      val senseMap = senses.map(l => (l.persistent_id,l)).toMap
      implicit val makeAttestation =   GetResult[Attestation](r => getAttestation(r))
  
      val q = s"""
      select 
           analyzed_wordform_id,
           quote, start_pos,
           end_pos, 
           d.eg_id,
           d.author,
           d.title,
           d.year_from,
           d.year_to,
           d.dictionary
      from
            ${dataSchema}.token_attestations t, ${senseSchema}.eg_sense e, ${senseSchema}.documents d
      where
            d.eg_id = e.eg_id
            and t.document_id=d.document_id
            and e.sense_id in """ + stringValues(ids)
      val a =  (db:Handle) => db.createQuery(q).map(makeAttestation)
      a
    } 
    
   // as in: concat(sql"select id from USERS where id in ", values(Seq(1,2))).as[Int]
}

object Hilex 
{
 
  case class Configuration(name: String, server: String, database: String, user:String, password: String)
  
  val hilexAtHome = Configuration(
        name="gigant_hilex", 
        server="svowdb02", 
        database="gigant_hilex_dev", 
        user="postgres", 
        password="inl")
        
  val diamantAtHome = hilexAtHome.copy(database="diamant_vuilnisbak")
  
  implicit lazy val ec = new ExecutionContext
  {
    val threadPool = Executors.newFixedThreadPool(5);

    def execute(runnable: Runnable)
    {
      threadPool.submit(runnable)
    }

    def reportFailure(t: Throwable) {}
  }
  
  val dev=true
  
  def makeHandle(conf:  Configuration) =
  {
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
  
  
  
  lazy val diamantRuwDB = makeHandle(diamantAtHome)
    
  
  implicit lazy val hilexDB = makeHandle(hilexAtHome)
  

  
  
   def slurp[A] (a: queries.AlmostQuery[A], db: Handle):List[A] =
    { 
       a(db).list().asScala.toList
    } 
   
  def slurp[A] (a: queries.AlmostQuery[A]):List[A] = slurp(a,hilexDB)
    
   import java.io._
        
  def pickleTo(l:List[Concordance], fileName:String) = 
  {

    val pw = new PrintWriter(new File(fileName ))
    val z = l.toArray.pickle.toString
    pw.write(z)
    pw.close
  }
  
  def findSomeLemmata:List[Lemma] =
  {
     slurp(queries.lemmaQueryWhere("modern_lemma ~ '^zin$'"))
  }
  
  def main(args:Array[String]):Unit = 
  { 
    val myLemma = "M089253"
    val l = slurp(queries.lemmaQueryWhere(s"persistent_id='${myLemma}'"))
    val zin = l.head
    println(zin)
    val senses = zin.senses
    val romans = senses.filter(s => s.parent_sense_id == null)
    romans.foreach(println)
    
    val attestationsAsConcordances  = romans.flatMap(
        s => queries.getAttestationsBelow(s)
          .map(s => s.toConcordance)
          .map(c =>  c.copy(metadata=c.metadata ++ List("senseId" ->  s.persistent_id, "lempos" -> "zin:n", ("id", ConvertOldInstanceBase.uuid))) 
          .tag(babTagger) )
      ).filter(_.hitStart > -1)
      
    attestationsAsConcordances.foreach(println)
    println(attestationsAsConcordances.size)
    //pickleTo(attestationsAsConcordances,s"Data/${myLemma}.pickle")
    //tester.leaveOneOut(new Swsd, attestationsAsConcordances)
    
   
  
    return
    
    
    l.foreach(x => x.senses.foreach(println))
    l.foreach(_.senses.foreach(s => 
      { println(s"\nAttestaties voor ${s}"); s.attestations.foreach( a => println(a.toConcordance.tag(babTagger).vertical)) }))
    val qs = queries.getSenses(l)
  
    val q = queries.getWordforms(l)
    val l1 = slurp(q,Hilex.hilexDB)
    l1.foreach(println)
    val q2 = queries.getAttestations(l1)
    val l2 = slurp(q2,Hilex.hilexDB)
    l2.foreach(println)
  }
}

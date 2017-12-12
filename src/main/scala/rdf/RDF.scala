package rdf

import org.apache.jena.graph.Triple
import org.apache.jena.query._
import org.apache.jena.arq._
import org.apache.jena.rdf.model.Model
import org.apache.jena.sparql.engine.http.QueryEngineHTTP
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._

class RDF
{
  val serviceURI = "http://dbpedia.org/sparql"
  val diamantURI = "http://svprre02:8080/fuseki/tdb/sparql"


  def convertTriple(t:Triple):Statement =
  {
    val subject = Resource(t.getSubject.toString)
		val predicate = Predicate(t.getPredicate.toString)
		val statement =
						  if (t.getObject.isLiteral)
						  {
						    val o = (t.getObject).asInstanceOf[org.apache.jena.graph.Node_Literal]
						    val tp = o.getLiteralDatatype
						    val v = o.getLiteralValue.toString()
						    DataProperty (
						        subject,
						        predicate,
						        if (tp.toString.contains("integer")) IntLiteral(v.toInt) else  StringLiteral(v))
						  } else
						  {
						    ObjectProperty(subject,predicate, Resource(t.getObject().toString()))
						  }

     statement
   }

  def constructQuery(service: String, queryString: String):Stream[Statement] =
  {
    val query = QueryFactory.create(queryString)
    try
    {
    	val qexec = new QueryEngineHTTP(service, query)
    			val i =	qexec.execConstructTriples()
    			val i1 = i.asScala.map(x => convertTriple(x)).toStream
    			i1
    } catch {case ex: Exception => ex.printStackTrace(); Stream.Empty}
  }

	def selectQuery(service: String, queryString: String):Stream[Map[String,String]] =
	{
		val q = QueryFactory.create(queryString)

		val qexec = new QueryEngineHTTP(service, q)
		val results = qexec.execSelect.asScala.toStream.map(soln => soln.varNames().asScala.map(n => n -> soln.get(n).toString).toMap)

		results
	}

  def query(service: String, queryString: String) =
	{
		val query = QueryFactory.create(queryString)
		val start = System.currentTimeMillis()
		try
		{
		  val qexec = new QueryEngineHTTP(service, query)


			if (query.isSelectType)
			{
				val results = qexec.execSelect
				while (results.hasNext)
				{
					val soln = results.nextSolution()
					System.out.println(soln)
					//RDFNode x = soln.get("varName") ;       // Get a result variable by name.
					//Resource r = soln.getResource("VarR") ; // Get a result variable - must be a resource
					//Literal l = soln.getLiteral("VarL") ;   // Get a result variable - must be a literal
				}
			} else if (query.isConstructType)
			{
				val i =	qexec.execConstructTriples()
				def i1 = i.asScala.map(convertTriple).toStream

				for (statement <- i1.take(100))
				  println(statement)

			}
		} catch {case ex: Exception => ex.printStackTrace();}

	  val end =  System.currentTimeMillis()
		System.err.println("Time: " + (end-start))
	}
}

object RDF
{
    val example = "construct  {?x a \"<http://dog>\" } where {?x a ?Concept} LIMIT 10000"
    val att = """
         construct { ?a <d:word> ?wf . ?a <d:quote> ?q . ?a <d:b> ?b . ?a <d:e> ?e   . ?a <d:t> ?t  . ?E <d:f> ?f }
where
{
  ?E <http://lemon-model.net/lemon#lexicalForm> ?f .
  ?f <http://www.ivdnt.org/diamant#attestation> ?a .
  ?f <http://lemon-model.net/lemon#writtenRep> ?wf .
  ?a a <http://www.ivdnt.org/diamant#hilex.Attestation> .
  ?a <http://www.ivdnt.org/diamant#beginIndex> ?b .
  ?a <http://www.ivdnt.org/diamant#endIndex> ?e .
  ?a <http://www.ivdnt.org/diamant#text> ?t .
  ?t <http://www.ivdnt.org/diamant#quotationText> ?q
} limit 10000"""

	 val AAmetNietTrivialeInflectie =
  """
     PREFIX diamant:      <http://www.inl.nl/diamant/>
 | 	 PREFIX lemon:  <http://lemon-model.net/lemon#>
 |   PREFIX lexinfo: <http://www.lexinfo.net/ontology/2.0/lexinfo#>
 |   PREFIX pwn: <http://wordnet-rdf.princeton.edu/ontology#>
 |
 |    select
 |
 |     ?metE ?zonderE
 |
 |
 |    {
 |		  ?e a lemon:LexicalEntry .
 |      ?e lemon:lexicalForm ?f .
 |      ?e lemon:canonicalForm ?lf .
 |      ?f lemon:writtenRep ?metE .
 |      ?lf lemon:writtenRep ?zonderE .
 |
 |      ?f pwn:part_of_speech ?p .
 |  		FILTER regex(?p, "AA.*pos*.*infl=e.*") .
 |      FILTER regex(?metE, ".*e$") .
 |      FILTER regex(?metE, ".*e$") .
 |      FILTER regex(?zonderE, ".*[^e]$")
 |    } limit 18000
  """.stripMargin
    def main(args:Array[String]) =
		{
			val r = (new RDF);
			r.selectQuery(r.diamantURI, AAmetNietTrivialeInflectie).zipWithIndex.foreach(println)
		}
}
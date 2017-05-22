import org.apache.jena.graph.Triple
import org.apache.jena.query._
import org.apache.jena.rdf.model.Model
import org.apache.jena.sparql.engine.http.QueryEngineHTTP
import scala.collection.JavaConverters._

trait RDFNode
trait RDFLiteral[+T] extends RDFNode
trait RDFResource

case class IntLiteral(i:Int) extends RDFNode with RDFLiteral[Int]
case class StringLiteral(s:String) extends RDFNode with RDFLiteral[String]
case class DoubleLiteral(s:Double) extends RDFNode with RDFLiteral[Double]
case class Resource(uri:String) extends RDFNode with RDFResource
case class Predicate(uri:String) extends RDFNode with RDFResource

trait Statement
case class ObjectProperty(subject:RDFResource, predicate:Predicate, obj:RDFResource) extends Statement
case class DataProperty(subject:RDFResource, predicate:Predicate, obj:RDFLiteral[Any]) extends Statement

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
    }
		 catch {case ex: Exception => ex.printStackTrace(); Stream.Empty}
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
				val i1 = i.asScala.map(convertTriple).toStream
			
				for (statement <- i1.take(10))
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
  ?a a <http://www.ivdnt.org/diamant#Attestation> . 
  ?a <http://www.ivdnt.org/diamant#beginIndex> ?b .
  ?a <http://www.ivdnt.org/diamant#endIndex> ?e .
  ?a <http://www.ivdnt.org/diamant#text> ?t .
  ?t <http://www.ivdnt.org/diamant#quotationText> ?q
} limit 10000"""
    def main(args:Array[String]) = { val r = (new RDF); r.query(r.diamantURI, att) }
}
import org.apache.jena.graph.Triple
import org.apache.jena.query._
import org.apache.jena.rdf.model.Model
import org.apache.jena.sparql.engine.http.QueryEngineHTTP

class RDF
{
  val serviceURI = "http://dbpedia.org/sparql"
  val diamantURI = "http://svprre02:8080/fuseki/tdb/sparql"
  
  def query(service: String, queryString: String) = 
	{
		val query = QueryFactory.create(queryString) ;
		val start = System.currentTimeMillis();
		try  
		{
		  val qexec = new QueryEngineHTTP(service, query)
		  
		  //val qexec = QueryExecutionFactory.create(query, dataset)
			if (query.isSelectType)
			{
				val results = qexec.execSelect 
				while (results.hasNext())
				{
					val soln = results.nextSolution() ;
					System.out.println(soln);

					//RDFNode x = soln.get("varName") ;       // Get a result variable by name.
					//Resource r = soln.getResource("VarR") ; // Get a result variable - must be a resource
					//Literal l = soln.getLiteral("VarL") ;   // Get a result variable - must be a literal
				}  
			} else if (query.isConstructType())
			{
				val i =	qexec.execConstructTriples();
				while (i.hasNext())
				{
					val t = i.next();
					try
					{
						System.out.println(t.getSubject() + "\t"  + t.getPredicate() + "\t"+  t.getObject());
					} 
					catch {case ex: Exception => ex.printStackTrace();}
				}
			}
		} catch {case ex: Exception => ex.printStackTrace();}
		
	  val end =  System.currentTimeMillis();
		System.err.println("Time: " + (end-start));
	}
  

    
}

object RDF
{
    val example = "construct  {?x a \"<http://dog>\" } where {?x a ?Concept} LIMIT 100"
    val att = """
         construct { ?a <word> ?wf . ?a <quote> ?q . ?a <b> ?b . ?a <e> ?e   . ?a <t> ?t } 
where 
{ 
  ?f <http://www.ivdnt.org/diamant#attestation> ?a .
  ?f <http://lemon-model.net/lemon#writtenRep> ?wf .
  ?a a <http://www.ivdnt.org/diamant#Attestation> . 
 ?a <http://www.ivdnt.org/diamant#beginIndex> ?b .
?a <http://www.ivdnt.org/diamant#endIndex> ?e .
 ?a <http://www.ivdnt.org/diamant#text> ?t .
 ?t <http://www.ivdnt.org/diamant#quotationText> ?q
} limit 100"""
    def main(args:Array[String]) = { val r = (new RDF); r.query(r.diamantURI, att) }
}
import org.apache.jena.graph.Triple;
import org.apache.jena.query._;
import org.apache.jena.rdf.model.Model;


class RDF
{
  val dataset:Dataset = null
  def query(queryString: String) = 
	{

		val query = QueryFactory.create(queryString) ;
		val start = System.currentTimeMillis();
		try  
		{
		  val qexec = QueryExecutionFactory.create(query, dataset)
			if (query.isSelectType())
			{
				val results = qexec.execSelect() ;
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
					} catch (Exception ex)
					{
						ex.printStackTrace();
					}
				}
			}
		}
		
	  val end =  System.currentTimeMillis();
		System.err.println("Time: " + (end-start));
	}
}
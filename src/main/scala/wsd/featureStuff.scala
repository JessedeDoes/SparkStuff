package wsd

import impact.ee.classifier.{Distribution, Feature, StochasticFeature}
import word2vec.Vectors
import concordance.Concordance
import collection.JavaConverters._

object featureStuff
{
    @volatile var vectorz = Vectors.readFromFile("/home/jesse/workspace/Diamant/Vectors/dbnl.vectors.bin")
    import word2vec.Distance.cosineSimilarity
    class MyFeature(n: String, f: Concordance=>String) extends Feature with Serializable
  	{
  	  this.name = n
  	  val fun = f

  	  override def getValue(o:Any) = o match { case r:Concordance => fun(r) }
  	}

  	class MyStochasticFeature(n:String, f:Concordance=>Distribution) extends StochasticFeature with Serializable
  	{
  	  this.name = n
  	  val fun = f
  	  override def getValue(o:Any):Distribution = o match { case r:Concordance => fun(r) }
  	}

  	def fieldAt(f:String,i:Int)(r:Concordance):String =
  	{
  	  val tokens = r.apply(f)
  	  val p = r.hitStart+i
  	  if (p > 0 && p < tokens.length) tokens(p) else "#"
  	}

  	def fieldFeature(n:String, f:String, i:Int) = new MyFeature(n,fieldAt(f,i))

  	def fieldAtMultiple(f:String, l:List[Int])(r:Concordance): String =
  	{
  	  val tokens = r.apply(f)
  	  val hs = r.hitStart
  	  val ps = l.map( hs +_ ).map(p =>  { if (p > 0 && p < tokens.length) tokens(p) else "#"} )
  	  ps.mkString("_")
  	}

  	def bowFeature(k:Int)(r:Concordance):Distribution =
  	{

  	  val tokens = r.apply("word")
  	  val p = r.hitStart
  	  val l = ((p-k to p-1).toList ++ (p+1 to p+k).toList).filter(i => i>0 && i < tokens.length)

  	  val d = new Distribution
  	  l.map(i => tokens(i)).foreach(d.incrementCount(_))
  	  d.computeProbabilities
  	  d
  	}

  	def vectorFeature(vectors:Vectors)(r:Concordance):Distribution =
  	{
  	   val window = 8
  	   val tokens = r.apply("word").toList.asJava
  	   val focus = r.hitStart
  	   val posFrom = Math.max(focus-window,0)
  	   val posTo = Math.min(focus+window+1,tokens.size)
  	   val vector = word2vec.Util.getRankedAndDistanceWeightedAverageVector(vectors, tokens, focus, posFrom, posTo)
  	   // Console.err.println(vector)
  	   val d = new Distribution
  	   (0 to vector.length-1).foreach(i => d.addOutcome("v" + i, vector(i)))
  	   d
  	}

  	def vectorNorm(v: Array[Float]):Double = Math.sqrt(v.map(x => x*x).sum)

  	def averageVector(v: List[Array[Float]]):(Array[Float],Double) =
  	{
  	  val r = (0 to v.head.length-1).toArray.map(i => v.map(_(i)).sum)

  	  val N = vectorNorm(r)
  	  word2vec.Util.normalize(r)
  	  (r,N)
  	}

	  case class ConcordanceWithVector(concordance:Concordance, vector: Array[Float])

  	case class SenseGroup(members: List[ConcordanceWithVector], memberIds: Set[String] , average: Array[Float], norm:Double)
  	{
  	    def similarity(v: Array[Float], id: String):Double =
  	    {
  	        if (memberIds.contains(id))
  	            {
  	    	  	     val x1 =  average.map(norm * _)
  	    		       val x2 = x1.indices.map(i => x1(i).asInstanceOf[Float] - v(i)).toArray
  	    		       word2vec.Util.normalize(x2)
  	    		       cosineSimilarity(v,x2)
  	    	     } else
  	         cosineSimilarity(v,average)
  	    }

			  def maxSimilarity(v: Array[Float], id: String):Double =
				{
					//val l = members.filter(x => !(x.concordance.meta("id") == id))
					if (members.isEmpty) 0.asInstanceOf[Double] else
					{
						val x = members.par.maxBy(x => if (x.concordance.meta("id") == id) 0 else word2vec.Distance.cosineSimilarity(v,x.vector))
						cosineSimilarity(v,x.vector)
					}
				}
  	}

	  // dit werkt (nog??) niet zo erg....

  	def centroidFeature(vectors:Vectors,training: List[Concordance], heldOutIds:Set[String]): Concordance => Distribution =
  	{
  	  val window = 4

  	  def weightedAverage(r:Concordance):Array[Float] =
  	  {
  	    val focus = r.hitStart
  	    val posFrom = Math.max(focus-window,0)
  	    val tokens = r.apply("word").toList.asJava
  	    val posTo = Math.min(focus+window+1,tokens.size)
  	    word2vec.Util.getRankedAndDistanceWeightedAverageVector(vectors,  tokens, focus, posFrom, posTo)
  	  }

  	  val quotationVectors = training.map(r => ConcordanceWithVector(r, weightedAverage(r))) // we want to cache this

  	  val filtered = quotationVectors.filter(x => !heldOutIds.contains(x.concordance.meta("id")))

  	  val groupCenters = filtered.groupBy(x => x.concordance.meta("senseId")).mapValues(l => averageVector(l.map(_.vector)) match { case (v,n) =>
				SenseGroup(l, l.map(_.concordance.meta("id")).toSet, v, n ) })

  	  def f(r:Concordance):Distribution =
  	  {
  	    val a = weightedAverage(r)
  	    val id = r.meta("id")

  	    val distances = groupCenters.mapValues(x => x.maxSimilarity(a,id))
  	    val N = distances.values.sum
  	    // Console.err.println("Distances:" + distances)
  	    val d = new Distribution
  	    distances.foreach( { case (k,v) => d.addOutcome(k, v / N) } )
  	    d
  	  }
  	  f
  	}
}

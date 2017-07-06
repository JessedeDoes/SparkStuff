import impact.ee.classifier.Classifier;
import impact.ee.classifier.Dataset;
import impact.ee.classifier.FeatureSet;
import impact.ee.classifier.Feature;
import impact.ee.classifier.Instance;
import impact.ee.classifier.Distribution;
import impact.ee.classifier.StochasticFeature;
import impact.ee.classifier.libsvm.LibLinearClassifier;
import impact.ee.classifier.libsvm.LibSVMClassifier;
import impact.ee.classifier.svmlight.SVMLightClassifier;
import impact.ee.classifier.svmlight.SVMLightClassifier.TrainingMethod;

import impact.ee.tagger.features.ClusterFeature;
import word2vec.Util
import word2vec.Vectors

import wsd.features.BoCLFeature;
import wsd.features.BoWFeature;
import wsd.features.ClusterAtFeature;
import wsd.features.LemmaAtFeature;
import wsd.features.PoSAtFeature;
import wsd.features.WordAtFeature;
import wsd.WSDInstance
import scala.collection.JavaConverters._

import org.apache.spark.{SparkConf,SparkContext}

import org.apache.spark.SparkContext._
import org.apache.spark.rdd.JdbcRDD
import org.apache.spark.sql._
import org.apache.spark.sql.types._
import org.apache.spark.sql.functions._

trait WSD
{
	def train(instances: List[Concordance], heldout: Set[String]): Concordance=>String = ???

}

object wsdObject extends WSD
{
  def tag(in: Stream[Concordance], tagger: Concordance=>String):Stream[Concordance] = in.map(c => c.copy(metadata=c.metadata + ("senseId" -> tagger(c))))
}

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
					val l = members.filter(x => !(x.concordance.meta("id") == id))
					if (l.isEmpty) 0.asInstanceOf[Double] else
					{
						val x = l.maxBy(x => word2vec.Distance.cosineSimilarity(v,x.vector))
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

class DistributionalOnly extends WSD
{
	import featureStuff._
	override def train(instances: List[Concordance], heldout: Set[String]): Concordance=>String =
	{
		val cf = centroidFeature(vectorz, instances, heldout)
		c =>
			{
				val D:Distribution = cf(c)
				//println(c.meta("senseId") + ", " + D)

				val oMax =  D.outcomes.asScala.maxBy(o => o.p)
				//println("oMax:" + oMax)
				oMax.label
			}
	}
}

class Swsd extends WSD with Serializable
{
    import featureStuff._
  
  	val addVectors = true
  	val addCentroids = true
  	
  	def makeFeatures():FeatureSet = 
  	{
  		
  	  val features = new FeatureSet
  	  val fList = 
  	  List( 
  	     fieldFeature("w0", "word", 0),
  	     fieldFeature("w-1", "word", -1),
  	     fieldFeature("w+1", "word", 1),
  	     fieldFeature("w-2", "word", -2),
  	     fieldFeature("w+2", "word", 2),
  	     fieldFeature("p-1", "pos", -1),
  	     fieldFeature("p+1", "pos", 1),
  	     fieldFeature("p-2", "pos", -2),
  	     fieldFeature("p+2", "pos", 2),
  	     fieldFeature("p0", "pos", 0)
  	  )
  	  
  	  fList.foreach(features.addFeature(_))
  	  features.addStochasticFeature(new MyStochasticFeature("bow8", bowFeature(8)))
  	  if (addVectors)
  	  {
  	    features.addStochasticFeature(new MyStochasticFeature("contextVector", vectorFeature(vectorz))) 
  	  }
  	  features
  	}
  	
   override def train(instances: List[Concordance], heldout: Set[String]): Concordance=>String =
	 {

		 
     val features = makeFeatures
     if (addCentroids) features.addStochasticFeature(new MyStochasticFeature("centerDistances", centroidFeature(vectorz,instances,heldout) ))
		 val classifier = new LibSVMClassifier
		
		
		 val d = new Dataset("trainingData")
		 d.features = features
		 features.finalize() // only necessary for continuous feats ......
		

		 for (w <- instances)
		 {
		   val id = w.meta("id")
		   if (!heldout.contains(id))
				 d.addInstance(w, w.meta("senseId"))
		 }
		 Console.err.println("start training...")
		 classifier.train(d)
		
		 return (r:Concordance) => classifier.classifyInstance(features.makeTestInstance(r))
	 }
}

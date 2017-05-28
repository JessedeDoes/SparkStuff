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

object featureStuff
{
    @volatile var vectorz = Vectors.readFromFile("/home/jesse/workspace/Diamant/Vectors/dbnl.vectors.bin")
    
    class MyFeature(n: String, f: Row=>String) extends Feature with Serializable
  	{
  	  this.name = n
  	  val fun = f
  	  
  	  override def getValue(o:Any) = o match { case r:Row => fun(r) }
  	}
  	
  	class MyStochasticFeature(n:String, f:Row=>Distribution) extends StochasticFeature with Serializable
  	{
  	  this.name = n
  	  val fun = f
  	  override def getValue(o:Any):Distribution = o match { case r:Row => fun(r) }
  	}
  	
  	def fieldAt(f:String,i:Int)(r:Row):String = 
  	{
  	  val tokens = r.getAs[Seq[String]](f)
  	  val p = r.getAs[Int]("hitStart")+i
  	  if (p > 0 && p < tokens.length) tokens(p) else "#"
  	}
  	
  	def fieldFeature(n:String, f:String, i:Int) = new MyFeature(n,fieldAt(f,i))
  	
  	def bowFeature(k:Int)(r:Row):Distribution =
  	{
  	  
  	  val tokens = r.getAs[Seq[String]]("word")
  	  val p = r.getAs[Int]("hitStart")
  	  val l = ((p-k to p-1).toList ++ (p+1 to p+k).toList).filter(i => i>0 && i < tokens.length)
  	  
  	  val d = new Distribution
  	  l.map(i => tokens(i)).foreach(d.incrementCount(_))
  	  d.computeProbabilities
  	  d
  	}
  	
  	def vectorFeature(vectors:Vectors)(r:Row):Distribution =
  	{
  	   val window=2
  	   val tokens = r.getAs[Seq[String]]("word").asJava
  	   val focus = r.getAs[Int]("hitStart")
  	   val posFrom = Math.max(focus-window,0)
  	   val posTo = Math.min(focus+window+1,tokens.size)
  	   val vector = word2vec.Util.getRankedAndDistanceWeightedAverageVector(vectors, tokens, focus, posFrom, posTo)
  	   // Console.err.println(vector)
  	   val d = new Distribution
  	   (0 to vector.length-1).foreach(i => d.addOutcome("v" + i, vector(i)))
  	   d
  	}
  	
  	def vectorNorm(v: Array[Float]):Double = Math.sqrt(v.map(x => x*x).sum);
  	
  	def averageVector(v: List[Array[Float]]):Array[Float] =
  	{
  	  def rowsum = (i:Int) => v.foldLeft(0.0f)( (k,a) => k + a(i))
  	  //println("Dim:" + v.head.length)
  	  val r = (0 to v.head.length-1).toArray.map(rowsum)
  	  //println(r.toList)
  	  
  	  word2vec.Util.normalize(r)
  	  r
  	}
  	
  	def centroidFeature(vectors:Vectors,training: List[Row], heldOutIds:Set[String]): Row => Distribution = 
  	{
  	  val window = 4
  	  
  	  def avg(r:Row):Array[Float] = 
  	  { 
  	    val focus = r.getAs[Int]("hitStart")
  	    val posFrom = Math.max(focus-window,0)
  	    val tokens = r.getAs[Seq[String]]("word").asJava 
  	    val posTo = Math.min(focus+window+1,tokens.size)
  	    word2vec.Util.getRankedAndDistanceWeightedAverageVector(vectors,  tokens, focus, posFrom, posTo) 
  	  }
  	  
  	  val quotationVectors = training.map(r => (r.getAs[String]("id"), r.getAs[String]("senseId"), avg(r))) // we want to cache this
  	  
  	  val filtered = quotationVectors.filter(x => !heldOutIds.contains(x._1)) // .toMap
  	  
  	  val removeMe = filtered.size < quotationVectors.size;
  	  println("quotation Vectors:"  + quotationVectors.length)
  	  
  	  val groupCenters = filtered.groupBy(_._2).mapValues(l => l.map(_._3)).mapValues(averageVector)
  	  
  	  println("Group centers:"  + groupCenters)
  	  
  	  
  	  def f(r:Row):Distribution = 
  	  {
  	    val qavg = avg(r)
  	    val distances = groupCenters.mapValues(word2vec.Distance.cosineSimilarity(qavg, _)) // misleading as r can be in a group
  	    //Console.err.println(distances)
  	    val d = new Distribution
  	    distances.foreach( { case (k,v) => d.addOutcome(k, v) } )
  	    d.computeProbabilities 
  	    d
  	  }
  	  f
  	}
}

class Swsd extends Serializable
{
    import featureStuff._
  
  	val addVectors = true
  	
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
  	  features.addStochasticFeature(new MyStochasticFeature("bow3", bowFeature(3)))
  	  if (addVectors)
  	  {
  	    Console.err.println("Adding vectors!")
  	    Console.err.println("Dimension: " + vectorz.vectorSize)
  	    features.addStochasticFeature(new MyStochasticFeature("contextVector", vectorFeature(vectorz))) 
  	  }
  	  features
  		//features.addStochasticFeature(new BoWFeature(3)); 
  	}
  	
   def train(instances: List[Row], heldout: Set[String]): Row=>String = 
	 {
     val df:DataFrame = null;
     val features = makeFeatures
     // features.addStochasticFeature(new MyStochasticFeature("centerDistances", centroidFeature(vectorz,instances,heldout) ))
		 val classifier = new LibSVMClassifier();
		
		
		 val d = new Dataset("trainingData")
		 d.features = features
		 features.finalize // only necessary for continuous feats ......
		
		 
		 for (w <- instances)
		 {
		   val id = w.getAs[String]("id")
		   if (!heldout.contains(id))
				 d.addInstance(w, w.getAs[String]("senseId"))
			 else
			   Console.err.println("Held out: " + id + " " + w)
		 }
		 classifier.train(d)
		
		 return (r:Row) => classifier.classifyInstance(features.makeTestInstance(r))
	 }
}
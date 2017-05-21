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


object tester
{
  var totalItems = 0
	var	totalErrors = 0
	var totalFailures = 0
	var totalMfsErrors = 0
	
  def leaveOneOut(wsd:Swsd, df: DataFrame):Unit = 
	{
    Console.err.println("starting...")
    val ks = df.select("lempos").distinct.collect
    Console.err.println("############### " + ks.length)
    // df.groupBy("lempos")
    
    val df1 = df.repartition(1000, df.col("lempos"))
    var c = 0;
    df1.foreachPartition(i => c += 1)
    Console.err.println("Aantal partities: " + c)
    df1.foreachPartition(r => leaveOneOut(wsd,r))
		
		val totalAccuracy = (totalItems - totalErrors -totalFailures) / totalItems.asInstanceOf[Double];
		val mfs = 100; // MFSScore(ib);
		System.err.println("overall:  " + totalErrors + " of " + totalItems + " failures: "  + totalFailures  + " score: " + totalAccuracy + " (mfs " + mfs + ")");
	}

  
  def leaveOneOut(wsd:Swsd,instanceIterator:Iterator[Row]):Unit = 
	{
    val instances = instanceIterator.toList // Hm niet leuk, maar ja
    val grouped = instances.groupBy(_.getAs[String]("lempos"))
    grouped.foreach( { case (lp,group) => if (lp.endsWith(":n")) leaveOneOut(wsd,group) })
	}
  
  def leaveOneOut(wsd:Swsd,instances: List[Row]):Unit = 
	{  
    var errors = 0;
		var total = 0;
		var failures = 0;
		
    val senses = instances.map(_.getAs[String]("senseId")).distinct
    val lempossen = instances.map(_.getAs[String]("lempos")).distinct
  
    if (instances.size < 3 || senses.size < 2)
      return;
    
    val lempos = instances.head.getAs[String]("lempos")
    
    Console.err.println("#### Working on " + lempos)
    instances.foreach(Console.err.println(_))
    //return
    
   
    
		System.err.println("starting work on: " + lempos + " " + senses);
		
		for (w <- instances)
		{
		  System.err.println("Holding out for instance:" + w.getAs[String]("id") + " " + w);
			try
			{
				wsd.train(instances, Set(w.getAs[String]("id")));
				errors += wsd.test(Set(w));
			} catch 
			{ case e:Exception =>
	
				e.printStackTrace();
				failures += 1;
			}
			total += 1;
		}
		
		val accuracy = (total- errors -failures) / (total + 0.0);
		
		val senseDistribution = instances.groupBy(_.getAs[String]("senseId")).mapValues(_.size).toList.sortWith((a,b) => a._2 > b._2)
	
		val mfsProportion = senseDistribution.head._2 /  (instances.size + 0.0)
		val mfsErrors = instances.size - senseDistribution.head._2
	
		Console.err.println(lempos + "  |senses|: " + senseDistribution.size + "  "  + errors + " errors of " + total + " failures: "  + 
		    failures  + " score: " + accuracy + " distribution: " + senseDistribution + "  mfs: " + mfsProportion);
		
		Console.err.println("Accuracy: " + accuracy)
		
		totalItems += total
		totalErrors += errors
		totalMfsErrors += mfsErrors
		totalFailures += failures
	}  
}

object featureStuff
{
    
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
  	   //Console.err.println(vector)
  	   val d = new Distribution
  	   (0 to vector.length-1).foreach(i => d.addOutcome("v" + i, vector(i)))
  	   d
  	}
}

class Swsd extends Serializable
{
    import featureStuff._
  	var features = new FeatureSet
  	var classifier = new LibSVMClassifier
  	
  	def makeFeatures() = 
  	{
  		
  	  features = new FeatureSet
  	  val fList = 
  	  List( 
  	     fieldFeature("w-1", "word",-1),
  	     fieldFeature("w+1", "word",1),
  	     fieldFeature("w-1", "word",-2),
  	     fieldFeature("w+1", "word",2),
  	     fieldFeature("p-1", "pos",-1),
  	     fieldFeature("p+1", "pos",1),
  	     fieldFeature("p0", "pos",0)
  	  )
  	  
  	  fList.foreach(features.addFeature(_))
  	  features.addStochasticFeature(new MyStochasticFeature("bow3", bowFeature(3)))
  	  @volatile var vectorz = Vectors.readFromFile("/home/jesse/workspace/Diamant/Vectors/dbnl.vectors.bin")
  	  println(vectorz.vectorSize)
  	  features.addStochasticFeature(new MyStochasticFeature("contextVector", vectorFeature(vectorz)))
  		//features.addStochasticFeature(new BoWFeature(3)); 
  	}
  	
   def train(instances: List[Row], heldout: Set[String]) = 
	 {
     val df:DataFrame = null;
    
		 this.classifier = new LibSVMClassifier();
		 makeFeatures();
		
		 val d = new Dataset("trainingData");
		 d.features = features;
		 features.finalize();  // only necessary for continuous feats ......
		
		 //heldout.filter(($"id" > 1) || ($"name" isin ("A","B"))).show()
		 
		 for (w <- instances)
		 {
		   //import instances.sqlContext.implicits._
		   val id = w.getAs[String]("id")
		   if (!heldout.contains(id))
				 d.addInstance(w, w.getAs[String]("senseId"))
			 else
			   Console.err.println("Held out: " + id + " " + w)
		 }
		 classifier.train(d);
	 }
   
  	def test(instances: Set[Row]):Int =
  		{
  				val t = new Dataset("test");
  				var errors = 0;
  				for (w <- instances)
  				{
  					val instance = features.makeTestInstance(w);
  					//System.err.println(instance);
  					val label = classifier.classifyInstance(instance);
  					val truth = w.getAs[String]("senseId")
  					
  					//System.err.println("############################################################################### " + label);
  					val isOK = label.equalsIgnoreCase(truth)
  					Console.err.println(isOK + " " + label + "\ttruth:" + truth + "\t" + w)
  					if (!isOK)
  						errors = errors +1;
  				} 
  				return errors;
  		}
}
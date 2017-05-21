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
  def leaveOneOut(wsd:Swsd, df: DataFrame):Unit = 
	{
    Console.err.println("starting...")
    val ks = df.select("lempos").distinct.collect
    Console.err.println("############### " + ks.length)
    // df.groupBy("lempos")
		for (key <- ks)
		{
		  val lp = key.getAs[String]("lempos")
			if (lp.endsWith(":n")) //  && key.startsWith("bank:n"))
			{
			  Console.err.println("It is a noun...:" + key)
			  if (true)
			  {
				val V = df.filter(
				    r => 
				      {val x = r.getAs[String]("lempos"); 
				      x!= null && x == lp})
				Console.err.println("--------------------------------------------------- Size of V: " + V.count())
				if (false)
				{
				V.foreach(println(_));
				if (V.select("senseId").distinct().count > 1)	
				   leaveOneOut(V, lp, wsd);
				}
			  }
			}
		}
		val totalAccuracy = (totalItems - totalErrors -totalFailures) / totalItems.asInstanceOf[Double];
		val mfs = 100; // MFSScore(ib);
		System.err.println("overall:  " + totalErrors + " of " + totalItems + " failures: "  + totalFailures  + " score: " + totalAccuracy + " (mfs " + mfs + ")");
	}

  
  def leaveOneOut(instances:DataFrame, key:String, wsd:Swsd):Unit = 
	{
		var errors = 0;
		var total = 0;
		var failures = 0;
		
		System.err.println("starting work on: " + key);
		
		for (w <- instances)
		{
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
		
		//Counter<String> c = senseDistribution(instances);
		//double mfsProportion = c.get(c.keyList().get(0)) / (double) instances.size();
		//String distribution = c.values().toString();
		//System.err.println(key + "  |senses|: " + ib.nSenses(key) + "  "  + errors + " errors of " + total + " failures: "  + failures  + " score: " + accuracy + " distribution: " + distribution + "  mfs: " + mfsProportion);
		
		System.err.println("Accuracy: " + accuracy);
		
		totalItems += total;
		totalErrors += errors;
		totalFailures += failures;
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
  	  val tokens = r.getAs[Array[String]](f)
  	  val p = r.getAs[Int]("hitStart")+i
  	  if (p > 0 && p < tokens.length) tokens(p) else "#"
  	}
  	
  	def fieldFeature(n:String, f:String, i:Int) = new MyFeature(n,fieldAt(f,i))
  	
  	def bowFeature(k:Int)(r:Row):Distribution =
  	{
  	  
  	  val tokens = r.getAs[Array[String]]("word")
  	  val p = r.getAs[Int]("hitStart")
  	  val l = ((p-k to p-1).toList ++ (p+1 to p+k).toList).filter(i => i>0 && i < tokens.length)
  	  
  	  val d = new Distribution
  	  l.map(i => tokens(i)).foreach(d.incrementCount(_))
  	  d.computeProbabilities
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

  		//features.addStochasticFeature(new BoWFeature(3)); 
  	}
  	
   def train(instances: DataFrame, heldout: Set[String]) = 
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
				 d.addInstance(w, w.getAs[String]("senseId")); 
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
  					//System.err.println(label + "\t" + w.senseId + "\t" + w.plainSentence());
  					//System.err.println("############################################################################### " + label);
  					val isOK = label.equalsIgnoreCase(w.getAs[String]("senseId"));
  					if (!isOK)
  						errors = errors +1;
  				} 
  				return errors;
  		}
}
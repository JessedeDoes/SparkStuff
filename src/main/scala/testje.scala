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

import org.apache.log4j.Logger
import org.apache.log4j.Level

case class TestResult(nItems:Int, nErrors:Int, nMfsErrors:Int)
{
  def +(other:TestResult):TestResult = TestResult(nItems + other.nItems, nErrors + other.nErrors, nMfsErrors + other.nMfsErrors)
}

object tester
{
  var totalItems = 0
	var	totalErrors = 0
	var totalFailures = 0
	var totalMfsErrors = 0
	val minWordsinExample = 8
  val minExamplesInSense = 5
  val minAvgPerSense = 20.0
  
  def leaveOneOut(wsd:wsd, df: DataFrame):Unit =
	{
    Console.err.println("starting...")
    val ks = df.select("lempos").distinct.collect
    Console.err.println("############### " + ks.length)
    // df.groupBy("lempos")
    
    val df1 = df.repartition(1000, df.col("lempos"))
    var c = 0;
    df1.foreachPartition(i => c += 1)
    Console.err.println("Aantal partities: " + c)
    // df1.foreachPartition(r => leaveOneOut(wsd,r))
	}

  def senseDistribution(instances: Seq[Concordance]) = instances.groupBy(_.meta("senseId")).mapValues(_.size).toList.sortWith((a,b) => a._2 > b._2)
  
  def enoughData(instances: Seq[Concordance]):Boolean =
  {
      val sd = senseDistribution(instances)
      val minPerSense = sd.map(_._2).reduce( (a,b) => Math.min(a,b))
      val avgPerSense = sd.map(_._2).sum / sd.size.asInstanceOf[Double]
      avgPerSense > minAvgPerSense
  }
    
  def leaveOneOut(wsd:wsd,instances:List[Concordance]):Unit =
	{
   
    val grouped = instances.groupBy(_.meta("lempos"))
    grouped.par.foreach( 
        {
          case (lempos,group) => println("Maybe embark on:" + lempos); if (lempos.matches(".*(:n|:NOU)$")) leaveOneOutOneLempos(wsd,group)
        }) 
	}
  
  def filterABit(all_Instances: List[Concordance]) = 
  {
    val instancesX = all_Instances.filter(r => { val x = r("word"); x.size >= minWordsinExample} )
		val senseDistribMap = senseDistribution(instancesX).toMap
		instancesX.filter(r => { senseDistribMap( r.meta("senseId")) >= minExamplesInSense} ) 
  }

  def leaveOneOutOneLempos(wsd: wsd, all_Instances: List[Concordance]):Unit =
	{  
    var errors = 0
		var total = 0
		var failures = 0
		val retrain = !wsd.isInstanceOf[DistributionalOnly]
		
		val instances = filterABit(all_Instances)
		
		val senseDistrib = senseDistribution(instances)
    val senses = instances.map(_.meta("senseId")).distinct
 
  
    if (senses.size < 2 || !enoughData(instances))
      return;
    
    val lempos = instances.head.meta("lempos")
    

    Console.err.println("#### Working on " + lempos)
    
    
		System.err.println("starting work on: " + lempos + " " + senses)

		var classify: Concordance => String = null
		for (w <- instances)
		{
		  System.err.println("Holding out for instance:" + w.meta("id") + " " + w);
			try
			{
				if (retrain || classify == null)
					classify = wsd.train(instances, Set(w.meta("id")))
				errors += test(Set(w), classify)
			} catch 
			{ case e:Exception =>
	
				e.printStackTrace()
				failures += 1
			}
			total += 1
		}
		
		val accuracy = (total- errors -failures) / (total + 0.0)
		
	
	
		val mfsProportion = senseDistrib.head._2 /  (instances.size + 0.0)
		val mfsErrors = instances.size - senseDistrib.head._2
	
		Console.err.println(lempos + "  |senses|: " + senseDistrib.size + "  "  + errors + " errors of " + total + " failures: "  + 
		    failures  + " score: " + accuracy + " distribution: " + senseDistrib + "  mfs: " + mfsProportion);
		
		Console.err.println("Accuracy: " + accuracy)
		
		synchronized
		{
	  	totalItems += total
		  totalErrors += errors
		  totalMfsErrors += mfsErrors
		  totalFailures += failures
		}
	}  
  
  	def test(instances: Set[Concordance], classify: Concordance=>String):Int =
  		{
  				val t = new Dataset("test");
  				var errors = 0;
  				// instances.foldLeft(0)( (a,w) => a +  (if (classify(w) == w.meta("senseId")) 0 else 1))
  				for (w <- instances)
  				{
  					val label = classify(w)
  					val truth = w.meta("senseId")
  					
  					val isOK = label.equalsIgnoreCase(truth)
  					Console.err.println(isOK + " " + label + "\ttruth:" + truth + "\t" + w)
  					if (!isOK)
  						errors = errors +1;
  				} 
  				return errors;
  		}
}



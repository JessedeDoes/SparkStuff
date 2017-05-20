import impact.ee.classifier.Classifier;
import impact.ee.classifier.Dataset;
import impact.ee.classifier.FeatureSet;
import impact.ee.classifier.Feature;
import impact.ee.classifier.Instance;
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

object swsd
{
  	var features = new FeatureSet;
  	var classifier = new LibSVMClassifier;
  	
  	class aap extends Feature
  	{
  	  
  	   override def getValue(o:Any):String = "aap"
  	   
  	   def this(k:Int) =
  	   {
  	     this()
  	     this.name = "aap"  + k
  	   }
  	}
  	
  	def makeFeatures() = 
  	{
  		
  	  features = new FeatureSet;
  		features.addFeature(new WordAtFeature(1));
  		features.addFeature(new WordAtFeature(-1));
  		features.addFeature(new WordAtFeature(2));
  		features.addFeature(new WordAtFeature(-2));

  		features.addFeature(new PoSAtFeature(0));
  		features.addFeature(new PoSAtFeature(1));
  		features.addFeature(new PoSAtFeature(-1));

  		features.addStochasticFeature(new BoWFeature(3)); 
  	}
  	
   def train(instances:Set[WSDInstance], heldout: Set[WSDInstance]) = 
	 {
		this.classifier = new LibSVMClassifier();
		makeFeatures();
		
		val d = new Dataset("trainingData");
		d.features = features;
		features.finalize();  // only necessary for continuous feats ......
		
		for (w <- instances)
		{
			if (!heldout.contains(w))
			{
				d.addInstance(w, w.senseId);
			} 
		}
		 classifier.train(d);
	 }
   
   def test(instances: Set[WSDInstance]):Int =
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
				   val isOK = label.equalsIgnoreCase(w.senseId);
				   if (!isOK)
					   errors = errors +1;
			   } 
			   return errors;
	}
}
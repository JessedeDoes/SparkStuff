package wsd

import impact.ee.classifier.libsvm.LibSVMClassifier
import impact.ee.classifier.{Dataset, FeatureSet}
import concordance.Concordance

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
			 println(w)
		   val id = w.meta("id")
		   if (!heldout.contains(id))
				 d.addInstance(w, w.meta("senseId"))
		 }
		 Console.err.println("start training...")
		 classifier.train(d)

		 return (r:Concordance) => classifier.classifyInstance(features.makeTestInstance(r))
	 }
}

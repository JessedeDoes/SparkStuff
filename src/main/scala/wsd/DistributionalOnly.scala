package wsd

import impact.ee.classifier.Distribution
import concordance.Concordance
import collection.JavaConverters._

class DistributionalOnly extends wsd.WSD
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

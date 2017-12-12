package wsd
import collection.JavaConverters._
import concordance.Concordance

trait WSD
{
	def train(instances: List[Concordance], heldout: Set[String]): Concordance=>String = ???

}

package hilex

import Attestation._

case class Sense(lemma: Lemma, persistent_id: String, lemma_id: String, parent_sense_id: String, definition: String, sense_type: String) {
  lazy val attestations:List[Attestation] = Hilex.slurp(hilexQueries.getAttestationsForSense(List(this)))
  lazy val deepAttestations = hilexQueries.getAttestationsBelow(this)
  lazy val parentSense = Hilex.slurp(hilexQueries.getParentSenses(List(this)))
  lazy val subSenses = Hilex.slurp(hilexQueries.getSubSenses(List(this)))
  lazy val synonymDefinitions = hilexQueries.getSynonymDefinitions(this)
  lazy val quotationText = attestations.map(_.quote).mkString(" ")
  lazy val deepQuotationText = deepAttestations.map(_.quote).mkString(" ")
}

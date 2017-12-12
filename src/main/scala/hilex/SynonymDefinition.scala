package hilex

case class SynonymDefinition(sense_id: String, synonym: String) {
  lazy val sense = hilexQueries.getSense(sense_id)
  lazy val lemma = sense.lemma
}

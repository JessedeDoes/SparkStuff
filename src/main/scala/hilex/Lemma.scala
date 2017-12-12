package hilex

case class Lemma(modern_lemma: String, lemma_id: Int, persistent_id: String, pos: String) {
  lazy val wordforms:List[Wordform] = Hilex.slurp(hilexQueries.getWordforms(List(this)))
  lazy val senses:List[Sense] = Hilex.slurp(hilexQueries.getSenses(List(this)))
}

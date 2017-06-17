
object Cells {
  val s = nl.inl.blacklab.search.Searcher.open(new java.io.File("/media/jesse/Data/Diamant/CorpusEzel/"))

  /* ... new cell ... */

  val c = new Concordancer

  /* ... new cell ... */

  val cezel = c.concordancesWindowed(s, "[word='ezel']")

  /* ... new cell ... */

  val zomaareentje = cezel.drop(100).take(1).head

  /* ... new cell ... */

  import KBKwic._

  /* ... new cell ... */

  val lemmata = hilexQueries.getLemmaWithPoS("barmhartigheid","NOU")

  /* ... new cell ... */

  val wf = lemmata.flatMap(l => l.wordforms)

  /* ... new cell ... */

  LexiconService.getWordforms("wereld")

  /* ... new cell ... */

  testSRU.expandRestricted("wereld")

  /* ... new cell ... */

  val kb_bonzing = KBKwic.kwicResultsTagged(SingleTerm("bonzing"))

  /* ... new cell ... */

  def expandCQL(t: String) = s"""[word='${LexiconService.getWordforms(t).mkString("|")}']"""
  def searchExpanded(t: String) = c.concordancesWindowed(s,expandCQL(t))
  def tabelletje(t: String) = Concordance.toHTMLTable(searchExpanded(t).toList)
  def colloc(t: String) = Conc.collocations(s,searchExpanded(t))

  /* ... new cell ... */

  val dlb = searchExpanded("deliberatie")

  /* ... new cell ... */

  Concordance.toHTMLTable(dlb.toList)

  /* ... new cell ... */

  res25

  /* ... new cell ... */

  val dinges = searchExpanded("fris")

  /* ... new cell ... */

  tabelletje("braadworst")

  /* ... new cell ... */

  tabelletje("onderscheiding")

  /* ... new cell ... */

  colloc("klimaat")

  /* ... new cell ... */
}
                  
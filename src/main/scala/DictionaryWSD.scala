import Hilex.slurp

/**
  * Created by jesse on 6/6/17.
  */
object  DictionaryWSD
{
  def attestationToTaggedConcordance(a: Attestation, sense_id: String):Concordance =
  {
    val c = a.toConcordance
    c.copy(metadata=c.metadata ++ List("senseId" ->  sense_id, "lempos" -> "zin:n", ("id", ConvertOldInstanceBase.uuid))).retokenize(Tokenizer).tag(babTagger)
  }

  def attestationToConcordance(a: Attestation, sense_id: String, lempos: String):Concordance =
  {
    val c = a.toConcordance
    c.copy(metadata=c.metadata ++ List("senseId" ->  sense_id, "lempos" -> lempos, ("id", ConvertOldInstanceBase.uuid)))
  }

  def allWords(paragraph: String):Concordance =
  {
    val swsd = new Swsd
    val tagged = babTagger.tag(paragraph)
    val c0 = Concordance(hitStart=0, hitEnd=1, tokenProperties=tagged, metadata = Map("id" -> ConvertOldInstanceBase.uuid ) )
    val senseTags = (0 to tagged("word").length-1).map(i =>
      {
        val c = c0.copy(hitStart=i, hitEnd=i+1, metadata = Map("id" -> ConvertOldInstanceBase.uuid ))
        val lemma = c("lemma")(c.hitStart)
        val pos = c("pos")(c.hitStart)
        println(s"$lemma -- $pos")
        val lemmata = hilexQueries.getLemmaWithPoS(lemma, pos)
        println(lemmata)
        val senses = lemmata.flatMap(_.senses).filter(s => s.parent_sense_id == null)
        val attestationsAsConcordances  = senses.flatMap(
          s => hilexQueries.getAttestationsBelow(s).map(a => attestationToConcordance(a,s.persistent_id, lemma + ":" + pos))
        ).filter(_.hitStart > -1)
        if (attestationsAsConcordances.size > 10)
          {
        lazy val taggedConcordances = attestationsAsConcordances.par.map(_.tag(babTagger))
        val classifier = swsd.train(taggedConcordances.toList,Set.empty)
        val senseTag = classifier(c)
        senseTag} else "unknown"
      }
    )
    val cTagged = c0.copy(tokenProperties=c0.tokenProperties + ("senseId" -> senseTags.toArray))
    println(cTagged.vertical)
    cTagged
  }

  val idZin = "M089253"
  val idEzel = "M016273"

  val fullWSD = false

  val ezelMap = Map("M016273.bet.1" -> "beest", "M016273.bet.17" -> "persoon")
  def flattenEzel  (s:Sense):Sense = { val id = s.persistent_id; if (ezelMap.contains(id)) s.copy(persistent_id=ezelMap(id)) else s.copy(persistent_id="ding")}
  def flattenEzel (c:Concordance):Concordance = { val id = c.meta("senseId"); val nid =  if (ezelMap.contains(id)) ezelMap(id) else "ding"; c.copy(metadata = c.metadata - "senseId" + ("senseId" -> nid)) }

  def testWSD(lemmaId: String) =
  {

    val l = slurp(hilexQueries.lemmaQueryWhere(s"persistent_id='${lemmaId}'"))
    val theLemma = l.head
    println(theLemma)
    val senses = theLemma.senses
    val romans = senses.filter(s => s.parent_sense_id == null)


    romans.foreach(println)


    val attestationsAsConcordances  = romans.flatMap(
      s => hilexQueries.getAttestationsBelow(s).map(a => attestationToConcordance(a,s.persistent_id, theLemma.modern_lemma + ":" + theLemma.pos))
    ).filter(_.hitStart > -1)

    lazy val taggedConcordances = attestationsAsConcordances.par.map(_.tag(babTagger)) // Concordance.tagBatches(babTagger, attestationsAsConcordances)

    attestationsAsConcordances.foreach(println)
    println(attestationsAsConcordances.size)

    // Hilex.pickleTo(attestationsAsConcordances, s"Data/${myLemma}.quotations.pickle")

    if (fullWSD)
      tester.leaveOneOut(new Swsd, taggedConcordances.toList)
    else
      tester.leaveOneOut(new DistributionalOnly, attestationsAsConcordances)
  }

  def main(args:Array[String]):Unit =
  {
    //allWords("ik heb geen zin in ezels")
    testWSD(idZin)




    return
  }
}

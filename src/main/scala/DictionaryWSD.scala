import DictionaryWSD.fullWSD
import Hilex.slurp

/**
  * Created by jesse on 6/6/17.
  */
object  DictionaryWSD
{
  val idZin = "M089253"
  val idEzel = "M016273"

  val fullWSD = true
  val lemmaOnly = false

  def attestationToTaggedConcordance(a: Attestation, sense_id: String):Concordance =
  {
    val c = a.toConcordance
    c.copy(metadata=c.metadata ++ List("senseId" ->  sense_id, "lempos" -> "zin:n", ("id", a.eg_id + "." + a.start_pos)))
      .retokenize(Tokenizer)
      .tag(babTagger)
  }

  def attestationToConcordance(a: Attestation, sense_id: String, lempos: String):Concordance =
  {
    val c = a.toConcordance
    c.copy(metadata=c.metadata ++ List("senseId" ->  sense_id, "lempos" -> lempos, ("id", a.eg_id + "." + a.start_pos)))
  }

  implicit val lemmaLevel = false

  def getClassifierFor(lemmata: Seq[Lemma], lempos:String, fullWSD:Boolean)(implicit lemmaLevel:Boolean):Concordance => String =
  {
    val senses = lemmata.flatMap(_.senses).filter(s => s.parent_sense_id == null)
    val sense2lemma:Map[String,String] = lemmata.flatMap(l => l.senses.map(s => (s.persistent_id -> l.persistent_id) )).toMap

    val attestationsAsConcordances = senses.flatMap(
      s => hilexQueries.getAttestationsBelow(s).map(a => attestationToConcordance(a, s.persistent_id, lempos))
    ).filter(_.hitStart > -1)

    lazy val maybeFlatter =
      if (lemmaLevel && lemmata.size > 1)
        attestationsAsConcordances.map(c => c.copy(metadata = c.metadata - "senseId" + ("senseId" -> sense2lemma(c.meta("senseId")))))
      else
        attestationsAsConcordances

    lazy val taggedConcordances = maybeFlatter.par.map(_.tag(babTagger))
    if (fullWSD)
      (new Swsd).train(taggedConcordances.toList, Set.empty)
    else
      (new DistributionalOnly).train(maybeFlatter.toList, Set.empty)
  }

  def getClassifierForLemmaId(lemmaId: String, fullWSD:Boolean) =
  {
    val l:Lemma = hilexQueries.getLemmaByPersistentId(lemmaId)
    getClassifierFor(List(l), l.modern_lemma + ":" + l.pos, fullWSD)(false)
  }

  def getClassifierForLemPos(lemma: String, pos:String) =
  {
    val l = hilexQueries.getLemmaWithPoS(lemma,pos)
    println("Possible lemmata:" + l)
    getClassifierFor(l, lemma + ":" + pos, fullWSD)(true)
  }

  lazy val ezelaar = getClassifierForLemmaId(idEzel, false)
  lazy val bezinner = getClassifierForLemmaId(idZin, true)
  lazy val ballifier = getClassifierForLemPos("bal", "NOU")

  def allWords(paragraph: String):Concordance =
  {
    val swsd = new Swsd
    val tagged = babTagger.tag(paragraph)
    val c0 = Concordance(hitStart = 0, hitEnd = 1, tokenProperties = tagged, metadata = Map("id" -> ConvertOldInstanceBase.uuid))
    val senseTags = tagged("word").indices.map(i => {
      val c = c0.copy(hitStart = i, hitEnd = i + 1, metadata = Map("id" -> ConvertOldInstanceBase.uuid))
      val lemma = c("lemma")(c.hitStart)
      val pos = c("pos")(c.hitStart)

      val lemmata = hilexQueries.getLemmaWithPoS(lemma, pos)

      val senses = lemmata.flatMap(_.senses).filter(s => s.parent_sense_id == null)
      val attestationsAsConcordances = senses.flatMap(
        s => hilexQueries.getAttestationsBelow(s).map(a => attestationToConcordance(a, s.persistent_id, lemma + ":" + pos))
      ).filter(_.hitStart > -1)
      if (attestationsAsConcordances.size > 10) {
        lazy val taggedConcordances = attestationsAsConcordances.par.map(_.tag(babTagger))
        val classifier = swsd.train(taggedConcordances.toList, Set.empty)
        val senseTag = classifier(c)
        senseTag
      } else "unknown"
    }
    )
    val cTagged = c0.copy(tokenProperties=c0.tokenProperties + ("senseId" -> senseTags.toArray))
    println(cTagged.vertical)
    cTagged
  }


  val ezelMap = Map("M016273.bet.1" -> "beest", "M016273.bet.17" -> "persoon")
  val zinMap = Map(
    "M089253.bet.8" -> "I Richting; weg; wijze",
    "M089253.bet.16" -> "II Wensch; wil; streven; trek",
    "M089253.bet.83" -> "III Geest",
    "M089253.bet.256" -> "IV Hart, gemoed; gevoelens",
    "M089253.bet.378" -> "V (Meton.) Mensch; levend wezen; personage.",
    "M089253.bet.387" -> "VI Beteekenis; inhoud",
    "M089253.bet.423" -> "VII Nut; waarde; bestaansgrond")

  def flattenEzel  (s:Sense):Sense = { val id = s.persistent_id; if (ezelMap.contains(id)) s.copy(persistent_id=ezelMap(id)) else s.copy(persistent_id="ding")}
  def flattenEzel (c:Concordance):Concordance = { val id = c.meta("senseId"); val nid =  if (ezelMap.contains(id)) ezelMap(id) else "ding"; c.copy(metadata = c.metadata - "senseId" + ("senseId" -> nid)) }
  def flattenZin (c:Concordance):Concordance = { val id = c.meta("senseId"); val nid =  if (zinMap.contains(id)) zinMap(id) else "other"; c.copy(metadata = c.metadata - "senseId" + ("senseId" -> nid)) }
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
    testWSD(idEzel)




    return
  }
}

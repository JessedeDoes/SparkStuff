import Hilex.slurp

/**
  * Created by jesse on 6/6/17.
  */
object  DictionaryWSD
{
  def attestationToTaggedConcordance(a: Attestation, sense_id: String):Concordance =
  {
    val c = a.toConcordance
    c.copy(metadata=c.metadata ++ List("senseId" ->  sense_id, "lempos" -> "zin:n", ("id", ConvertOldInstanceBase.uuid))).tag(babTagger)
  }

  def attestationToConcordance(a: Attestation, sense_id: String):Concordance =
  {
    val c = a.toConcordance
    c.copy(metadata=c.metadata ++ List("senseId" ->  sense_id, "lempos" -> "zin:n", ("id", ConvertOldInstanceBase.uuid)))
  }

  def main(args:Array[String]):Unit =
  {
    val myLemma = "M089253"
    val l = slurp(hilexQueries.lemmaQueryWhere(s"persistent_id='${myLemma}'"))
    val zin = l.head
    println(zin)
    val senses = zin.senses
    val romans = senses.filter(s => s.parent_sense_id == null)
    romans.foreach(println)

    val attestationsAsConcordances  = romans.flatMap(
      s => hilexQueries.getAttestationsBelow(s).map(a => attestationToTaggedConcordance(a,s.persistent_id))
    ).filter(_.hitStart > -1)

    attestationsAsConcordances.foreach(println)
    println(attestationsAsConcordances.size)
    //pickleTo(attestationsAsConcordances,s"Data/${myLemma}.pickle")
    tester.leaveOneOut(new Swsd, attestationsAsConcordances)



    return
  }
}

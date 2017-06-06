import Hilex.slurp

/**
  * Created by jesse on 6/6/17.
  */
object  DictionaryWSD
{
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
      s => hilexQueries.getAttestationsBelow(s)
        .map(s => s.toConcordance)
        .map(c =>  c.copy(metadata=c.metadata ++ List("senseId" ->  s.persistent_id, "lempos" -> "zin:n", ("id", ConvertOldInstanceBase.uuid)))
          .tag(babTagger) )
    ).filter(_.hitStart > -1)

    attestationsAsConcordances.foreach(println)
    println(attestationsAsConcordances.size)
    //pickleTo(attestationsAsConcordances,s"Data/${myLemma}.pickle")
    tester.leaveOneOut(new Swsd, attestationsAsConcordances)



    return
  }
}

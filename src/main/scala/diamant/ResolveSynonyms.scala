package diamant

import hilex.{Sense, hilexQueries, _}
import embeddings._

object ResolveSynonyms
{
   val idZin = "M089253"
   val idEzel = "M016273"
   val idGramschap = "M021496"
   val idToon = "M069230"

   case class Scored[T](s1: T, score: Double)

   def withScore(f:(Sense,Sense)=>Option[Double]):(Sense,Sense)=>Scored[Sense] = {
      (s1, s2) =>
         f(s1, s2) match {
            case Some(d) => Scored(s2, d)
            case None => Scored(s2, -666)
         }
   }



  def distanceByDefinition(s1: Sense, s2: Sense):Option[Double] = DbnlVectors.similarityByAverage(s1.definition, s2.definition)


   def distanceByQuotation(s1: Sense, s2: Sense):Option[Double] =
      DbnlVectors.similarityByAverage(s1.definition + " " + s1.quotationText, s2.definition + " " + s2.quotationText)

   def distanceByDeepQuotation(s1: Sense, s2: Sense):Option[Double] =
    DbnlVectors.similarityByAverage(s1.definition + " " + s1.deepQuotationText, s2.definition + " " + s2.deepQuotationText)

   def doLemma(lemmaId: String): Unit =
    {
      import hilex.hilexQueries._
      val zin:Lemma = hilexQueries.getLemmaByPersistentId(lemmaId)
      val romansAndArabs = zin.senses.filter(romanOrArabic)
      val zinonyms = romansAndArabs.flatMap(s => s.synonymDefinitions)
      zinonyms.foreach(println)
      val possibleResolutions = zinonyms.map(
        z => (z, hilexQueries.getLemmaWithPoS(z.synonym,"NOU").flatMap(e =>e.senses.filter(romanOrArabic))  )) // of senses.filter(s => List("roman", "arabic").contains(s.sense_type))

      possibleResolutions.foreach({ case (z,l) => println(s"\n\n${z}"); l.foreach(println) } )

      val withSimilarities =
        possibleResolutions.map(
          {
            case (syn, l) =>
              (syn, syn.sense, l.map(s => withScore(distanceByDefinition)(syn.sense, s)).sortBy(-1 * _.score))
          }
        )
      withSimilarities.foreach(
        { case (syn, s, l) => println(s"\n\n#### ${syn} " + s); l.foreach(println) }
      )
    }
   def main(args:Array[String]):Unit =
   {
      doLemma(idEzel)
   }
}

/*


hilex.Sense(hilex.Lemma(ezel,197759,M016273,NOU),M016273.bet.32,M016273,null,In de boeventaal. Lessenaar; winkellade. ,arabic)
hilex.Sense(hilex.Lemma(ezel,197759,M016273,NOU),M016273.bet.34,M016273,null,Zijde van een dobbelsteen, waarop één oog voorkomt. In dit gebruik is ezel waarschijnlijk uit aas ontstaan. Verouderd. ,arabic)
hilex.Sense(hilex.Lemma(ezel,197759,M016273,NOU),M016273.bet.33,M016273,null,In de boeventaal. Foulard, das ( Boeventaal ).,arabic)
hilex.Sense(hilex.Lemma(ezel,197759,M016273,NOU),M016273.bet.21,M016273,null,Als benaming voor verschillende toestellen of voorwerpen dienende om iets te dragen of te steunen.,arabic)
hilex.Sense(hilex.Lemma(ezel,197759,M016273,NOU),M016273.bet.17,M016273,null,In toepassing op personen. ,arabic)
hilex.Sense(hilex.Lemma(ezel,197759,M016273,NOU),M016273.bet.1,M016273,null,Naam van enkele soorten van het geslacht der paarden, die zich vooral onderscheiden door een dikkeren kop, langere ooren en een staart die alleen aan de punt lange haren heeft. ,arabic)
 */
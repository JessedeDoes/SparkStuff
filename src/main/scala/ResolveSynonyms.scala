import word2vec.{Vectors,Util, Distance}


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
      import hilexQueries._
      val zin = hilexQueries.getLemmaByPersistentId(lemmaId)
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
      doLemma(idToon)
   }
}

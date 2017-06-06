object ResolveSynonyms
{
   val idZin = "M089253"

   def main(args:Array[String]) =
   {
      val zin = hilexQueries.getLemmaByPersistentId(idZin)
      val zinonyms = zin.senses.flatMap(s => s.synonymDefinitions)
      zinonyms.foreach(println)
      val possibleResolutions = zinonyms.map(
         z => (z, hilexQueries.getLemmaWithPoS(z.synonym,"NOU").flatMap(e =>e.senses)))
      possibleResolutions.foreach(println)
      val z = hilexQueries.getLemmaWithPoS("manier", "NOU")
      z.foreach(println)
   }
}

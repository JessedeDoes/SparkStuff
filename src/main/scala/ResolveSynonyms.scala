object ResolveSynonyms
{
   val idZin = "M089253"

   def main(args:Array[String]) =
   {
      val zin = hilexQueries.getLemmaByPersistentId(idZin)
      val zinonyms = zin.senses.flatMap(s => s.synonymDefinitions)
      println(zinonyms)
      val z = hilexQueries.getLemmaWithPoS("manier", "NOU")
      z.foreach(println)
   }
}

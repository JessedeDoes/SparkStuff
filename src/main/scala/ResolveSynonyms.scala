import word2vec.{Vectors,Util, Distance}

trait VectorSpace
{
   lazy val vectors:Vectors = ???
   type vector = Array[Float]
   def similarity(v1: Array[Float], v2: Array[Float]):Double =
      Distance.cosineSimilarity(v1,v2)
   def embedding(word: String) = vectors.getVector(word)
   def norm(v: vector):Double = Math.sqrt(v.map(x => x*x).sum)
   def normalized(v: vector): vector = v.map(f => f / norm(v).asInstanceOf[Float]) // gaat de optimizer deze herhaling weghalen ?
   def sum(v1: vector, v2:vector):vector = v1.zipWithIndex.map(p => p._1 + v2(p._2)) // silly
   def average(l: Seq[vector]):vector = l.reduce(sum)
}

object DbnlVectors extends VectorSpace
{
   lazy val vectors = Vectors.readFromFile("/home/jesse/workspace/Diamant/Vectors/dbnl.vectors.bin")


}
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

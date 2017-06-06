import word2vec.{Vectors,Util, Distance}

trait VectorSpace
{
   lazy val vectors:Vectors = ???
   type vector = Array[Float]
   def similarity(v1: Array[Float], v2: Array[Float]):Double =
      Distance.cosineSimilarity(v1,v2)
   def attemptNotNull[T](x:  () => T):Option[T] =
   {
      try
      {
         val z:T = x();
         if (z == null) None else Some(z)
      } catch { case e => println(e); None }
   }

   def preprocess(s: String) = Tokenizer.tokenize( s.replaceAll("<[^<>]*>","") ).map(t => entities.substitute(t.token)(entities.defaultMapping))
   def embedding(word: String):Option[vector] = { attemptNotNull(() => vectors.getVector(word)) }
   def norm(v: vector):Double = Math.sqrt(v.map(x => x*x).sum)
   def normalized(v: vector): vector = v.map(f => f / norm(v).asInstanceOf[Float]) // gaat de optimizer deze herhaling weghalen ?
   def sum(v1: vector, v2:vector):vector = v1.zipWithIndex.map(p => p._1 + v2(p._2)) // silly
   def average(l: Seq[vector]):vector = normalized(l.reduce(sum))
   def averageVector(l: Seq[String]):vector = average( l.map(embedding).filter(_ != None).map(_.get)  )
   def averageVector(s: String):vector = { val a = averageVector(preprocess(s));  a}
   def similarityByAverage(s1: String, s2: String) = attemptNotNull(() => similarity(averageVector(s1), averageVector(s2)))
}

object DbnlVectors extends VectorSpace
{
   override lazy val vectors = Vectors.readFromFile("/home/jesse/workspace/Diamant/Vectors/dbnl.vectors.bin")
}
object ResolveSynonyms
{
   val idZin = "M089253"

   case class Scored[T](s1: T, score: Double)

   def distanceByDefinition(s1: Sense, s2: Sense):Scored[Sense] =
      DbnlVectors.similarityByAverage(s1.quotationText, s2.quotationText) match
         {
           case Some(d) => Scored(s2,d)
           case None =>  Scored(s2,0.0)
         }

   def distanceByQuotation(s1: Sense, s2: Sense):Scored[Sense] =
      DbnlVectors.similarityByAverage(s1.definition, s2.definition) match
      {
         case Some(d) => Scored(s2,d)
         case None =>  Scored(s2,0.0)
      }

   def main(args:Array[String]):Unit =
   {
      val zin = hilexQueries.getLemmaByPersistentId(idZin)
      val zinonyms = zin.senses.flatMap(s => s.synonymDefinitions)
      zinonyms.foreach(println)
      val possibleResolutions = zinonyms.map(
         z => (z, hilexQueries.getLemmaWithPoS(z.synonym,"NOU").flatMap(e =>e.senses)))

      possibleResolutions.foreach({ case (z,l) => println(s"\n\n${z}"); l.foreach(println) } )

      val withSimilarities = possibleResolutions.map(
         { case (syn, l) => (syn, syn.sense,
                              l.map(s => distanceByDefinition(syn.sense, s)).sortBy(-1 * _.score)
                            )  }
      )
      withSimilarities.foreach(
         { case (syn, s, l) => println(s"\n\n#### ${syn} " + s); l.foreach(println) }
      )
      val z = hilexQueries.getLemmaWithPoS("manier", "NOU")
      z.foreach(println)
   }
}

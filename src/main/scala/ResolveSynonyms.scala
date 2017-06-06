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
      } catch { case e:Exception =>  None }
   }

   def lift2[T,S](f: (T,T) => S):(Option[T],Option[T]) => Option[S] =
      { (a:Option[T],b:Option[T]) => (a,b) match { case (Some(x), Some(y)) => Some( f(x,y)); case _ => None } }

   def preprocess(s: String) = Tokenizer.tokenize( entities.substitute(s.replaceAll("<[^<>]*>", "") )).map(t => t.token)
   def embedding(word: String):Option[vector] = { attemptNotNull(() => vectors.getVector(word)) }
   def norm(v: vector):Double = Math.sqrt(v.map(x => x*x).sum)
   def normalized(v: vector): vector = v.map(f => f / norm(v).asInstanceOf[Float]) // gaat de optimizer deze herhaling weghalen ?
   def sum(v1: vector, v2:vector):vector = (0 to v1.length -1).map(i => v1(i) + v2(i)).toArray // silly
   def average(l: Seq[vector]):vector = normalized(l.reduce(sum))

   def averageVector(l: Seq[String]):Option[vector] =
   {
      val okeetjes =  l.map(embedding).filter(_ != None).map(_.get)
      if (okeetjes.isEmpty)
         { Console.err.println("!No words found for:" + l.toList); None }
      else {
         Console.err.println(s"${okeetjes.size} for " + l.toList)
         Some(average(okeetjes))
      }
   }

   def averageVector(s: String):Option[vector] =
   {
      val a = averageVector(preprocess(s))
      if (a == None)
         Console.err.println(s"Average = None for <${s}>")
      a
   }

   def similarityByAverage(s1: String, s2: String):Option[Double] =
   {
      lift2(similarity)(averageVector(s1), averageVector(s2))
   }
}

object DbnlVectors extends VectorSpace
{
   override lazy val vectors = Vectors.readFromFile("/home/jesse/workspace/Diamant/Vectors/dbnl.vectors.bin")
}
object ResolveSynonyms
{
   val idZin = "M089253"

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
      DbnlVectors.similarityByAverage(s1.definition + " " + s1.quotationText, s2.definition + " " + s2.quotationText) match


   def main(args:Array[String]):Unit =
   {
      val zin = hilexQueries.getLemmaByPersistentId(idZin)
      val zinonyms = zin.senses.flatMap(s => s.synonymDefinitions)
      zinonyms.foreach(println)
      val possibleResolutions = zinonyms.map(
         z => (z, hilexQueries.getLemmaWithPoS(z.synonym,"NOU").flatMap(e =>e.senses.filter(s => List("roman", "arabic").contains(s.sense_type))  )))

      possibleResolutions.foreach({ case (z,l) => println(s"\n\n${z}"); l.foreach(println) } )

      val withSimilarities =
         possibleResolutions.map(
            {
               case (syn, l) => (syn, syn.sense,
                                    l.map(s => withScore(distanceByDefinition)(syn.sense, s)).sortBy(-1 * _.score))
            }
      )
      withSimilarities.foreach(
         { case (syn, s, l) => println(s"\n\n#### ${syn} " + s); l.foreach(println) }
      )
      val z = hilexQueries.getLemmaWithPoS("manier", "NOU")
      z.foreach(println)
   }
}

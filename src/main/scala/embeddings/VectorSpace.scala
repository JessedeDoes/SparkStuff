package embeddings

import tokenizer.{Tokenizer, entities}
import word2vec.{Distance, Vectors}

/**
  * Created by jesse on 6/11/17.
  */
trait VectorSpace {
  lazy val vectors: Vectors = ???
  type vector = Array[Float]

  def similarity(v1: Array[Float], v2: Array[Float]): Double =
    Distance.cosineSimilarity(v1, v2)

  def attemptNotNull[T](x: () => T): Option[T] = {
    try {
      val z: T = x()
      Option(z)
    } catch {
      case e: Exception => None
    }
  }

  def lift2[T, S](f: (T, T) => S): (Option[T], Option[T]) => Option[S] = { (a: Option[T], b: Option[T]) => (a, b) match {
    case (Some(x), Some(y)) => Some(f(x, y));
    case _ => None
  }
  }

  def preprocess(s: String): Array[String] = Tokenizer.tokenize(entities.substitute(s.replaceAll("<[^<>]*>", ""))).map(t => t.token)

  def embedding(word: String): Option[vector] = {
    attemptNotNull(() => vectors.getVector(word))
  }

  def norm(v: vector): Double = Math.sqrt(v.map(x => x * x).sum)

  def normalized(v: vector): vector = v.map(f => f / norm(v).asInstanceOf[Float]) // gaat de optimizer deze herhaling weghalen ?
  def sum(v1: vector, v2: vector): vector = v1.indices.map(i => v1(i) + v2(i)).toArray // silly
  def average(l: Seq[vector]): vector = normalized(l.reduce(sum))

  def averageVector(l: Seq[String]): Option[vector] = {
    val okeetjes = l.map(embedding).filter(_.isDefined).map(_.get)
    if (okeetjes.isEmpty) { //Console.err.println("!No words found for:" + l.toList);
      None
    }
    else {
      // Console.err.println(s"${okeetjes.size} for " + l.toList)
      Some(average(okeetjes))
    }
  }

  def averageVector(s: String): Option[vector] = {
    val a = averageVector(preprocess(s))
    if (a == None)
      Console.err.println(s"Average = None for <${s}>")
    a
  }

  def similarityByAverage(s1: String, s2: String): Option[Double] = {
    lift2(similarity)(averageVector(s1), averageVector(s2))
  }
}

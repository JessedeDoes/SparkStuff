import DatabaseUtilities._

/**
  * Created by jesse on 6/9/17.
  */
object SimpleTextStats
{
  case class Woordje(lemma:String, pos:String, id:String, wordform: String)
  def hilexHas() : String => Boolean =
  {

    val exampleQuery =
      Select(
        mapping = r => Woordje(
          r.getString("modern_lemma"),
          r.getString("lemma_part_of_speech"),
          r.getString("persistent_id"),
          r.getString("wordform")),
        from =
          """
            |data.lemmata l, data.analyzed_wordforms a, data.wordforms w where
            |l.lemma_id=a.lemma_id and w.wordform_id=a.wordform_id
             """.stripMargin)
     val allWords = Hilex.slurp(doeHet(exampleQuery)).groupBy(w => w.wordform.toLowerCase)

     s => { val b  =  allWords.contains(s.toLowerCase); /* if (!b) println(s); */ b}
  }

  lazy val f1:String => Boolean = hilexHas()

  def stats(text: String): Map[String,Any] =
  {
     val tokens = Tokenizer.tokenize(text)

     val n = tokens.length
     val n2 = tokens.count(t => f1(t.token))
     val nn = tokens.count(t => t.token.matches("^[0-9\\.,;]+$")) // getallen etc doen niet mee
     val nUc = tokens.count(t => t.token.matches("^[A-Z].*"))
     val p = n2 / (n - nn ).asInstanceOf[Double]
     if (n > 50 && p < 0.75)
       Console.err.println(text)
     Map("N" -> n, "nFound" -> n2, "p" -> p, "nNumerical" -> nn, "nUc" -> nUc)
  }

  def main(args:Array[String]):Unit =
  {
    scala.io.Source.fromFile(args(0)).getLines().foreach(l =>
      {
        val fields:Array[String] = l.split("\\t")
        val s = stats(fields(2))
        val nf  = fields ++ List(s("N").toString, s("p").toString)
        println (nf.mkString("\t"))
      }
    )
  }
    //println(stats("der zat een kat op de mat maatje xxxxxxxx"))
}

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
     //if (n > 50 && p < 0.75) Console.err.println(text)
     Map("N" -> n, "nFound" -> n2, "p" -> p, "nNumerical" -> nn, "nUc" -> nUc)
  }

  def main(args:Array[String]):Unit =
  {
    import scala.xml._
    val HTML = <html><head><meta http-equiv="content-type" content="text/html;charset=utf8"></meta></head><body><table>{
    scala.io.Source.fromFile(args(0)).getLines().map(l =>
      {
        val fields:Array[String] = l.split("\\t")
        val text = fields(2)
        // http://www.delpher.nl/nl/kranten/view?coll=ddd&identifier=MMSADB01:000012578:mpeg21:a0005&query=bunzing
        val id = fields(1).replaceAll(".*urn=","")
        // println(id)
        val linkje = s"http://www.delpher.nl/nl/kranten/view?coll=ddd&identifier=$id&query=bunzing"
        val link = <a href={linkje}>{fields(1)}</a>
        val s = stats(text)
        val snippets = KBKwic.concordance(SingleTerm(fields(0)), text)
        if (snippets.isEmpty)
          Console.err.println(s"Geen bunzing in $id:" + text)
        val nf = fields.slice(0,1)  ++ List(link, Concordance.toHTMLList(snippets)) ++ fields.slice(3,fields.length) ++ List(s("N").toString, s("p").toString)
        <tr valign="top">{nf.map(x => <td>{x}</td>)}</tr>
      })}
    </table></body></html>
    println(HTML)
  }
    //println(stats("der zat een kat op de mat maatje xxxxxxxx"))
}

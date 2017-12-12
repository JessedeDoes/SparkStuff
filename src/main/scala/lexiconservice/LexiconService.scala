package lexiconservice

import scala.xml._

case class LexiconService(serviceURL: String, database: String)
{
  def getWordforms(lemma: String):List[String] =
  {
    (XML.load(s"$serviceURL/get_wordforms?database=$database&lemma="
      + lemma.toLowerCase)
      \\ "found_wordforms").toList.map(_.text.toLowerCase).filter(_.matches("^(\\p{L})*$"))
  }

  def getLemmata(wordform: String):List[(String,String)] =
  {
    (XML.load(s"$serviceURL/get_lemma?database=$database&wordform=" + wordform.toLowerCase)
      \\ "found_lemmata").toList.map(e => ((e \ "lemma").text, (e \ "pos").text))
  }
}

object LexiconService extends LexiconService(defaults.serviceURL, defaults.database)
{
   def main(args: Array[String]) : Unit =
   {
     getWordforms("wereld").foreach(println)
     getLemmata("kipt").foreach(println)
   }
}
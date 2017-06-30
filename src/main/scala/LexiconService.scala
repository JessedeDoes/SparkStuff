import scala.xml._

object LexiconService
{
   val serviceURL = "http://sk.taalbanknederlands.inl.nl/LexiconService/lexicon"
   val database = "lexicon_service_db"

   def getWordforms(lemma: String):List[String] =
   {
     (XML.load(s"$serviceURL/get_wordforms?database=$database&lemma="
         + lemma.toLowerCase)
     \\ "found_wordforms").toList.map(_.text.toLowerCase).filter(_.matches("^(\\p{L})*$"))
   }
   
   def getLemmata(wordform: String):List[String] =
   {
     (XML.load(s"$serviceURL/get_lemmata?database=$database&wordform=" + wordform.toLowerCase)
     \\ "found_lemmata").toList.map(_.text.toLowerCase)
   }

   def main(args: Array[String]) : Unit =
   {
     getWordforms("wereld").foreach(println)
     getLemmata("kip").foreach(println)
   }
}
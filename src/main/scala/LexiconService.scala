import scala.xml._

object LexiconService
{
   def getWordforms(lemma: String):List[String] =
   {
     (XML.load("http://sk.taalbanknederlands.inl.nl/LexiconService/lexicon/get_wordforms?database=lexicon_service_db&lemma=" + lemma.toLowerCase)
     \\ "found_wordforms").toList.map(_.text.toLowerCase)
   }
   def getLemmata(wordform: String):List[String] =
   {
     (XML.load("http://sk.taalbanknederlands.inl.nl/LexiconService/lexicon/get_lemmata?database=lexicon_service_db&wordform=" + wordform.toLowerCase)
     \\ "found_lemmata").toList.map(_.text.toLowerCase)
   }
}
package kb

import lexiconservice.LexiconService

object testSRU
{
   import SRU._
   val expand: String => List[String] = LexiconService.getWordforms
   val expandRestricted: String => List[String]  = x => LexiconService.getWordforms(x).filter(QueryKB.getNumberOfResults(_) > 0)

   def main(args:Array[String]) =
   {
      val t0 = Phrase("goed", "gedachte")
      val t00 = Phrase("ernstig", "probleem")

      val t1 = expandQuery(expandRestricted)(t00)
      println(t1)
      println(termsIn(t1))
      println(t1.toQueryString())
      println(QueryKB.getNumberOfResults(t1))
      KBKwic.kwicResultsPar(t1)
   }
}

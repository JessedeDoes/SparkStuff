package tagger

import concordance.Concordance

object chnTagger extends Tagger
{
   override def urlFor(text:String) = s"http://openconvert.clarin.inl.nl/openconvert/text?input=${java.net.URLEncoder.encode(text)}&format=text&tagger=chn-tagger&output=raw&to=tei"

    def main(args:Array[String]):Unit =
    {
      val tagged = tag("""Je ben een ezel die zijn verstand kwijt is. Zwijg!""" )
      val c = Concordance(0,0,tagged,Map.empty)
      println(c.tokenProperties("lemma").toList)
      println(c.vertical)
    }
}

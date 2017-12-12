package tagger

import concordance.Concordance

object babTagger extends Tagger
{
   override def urlFor(text:String) = s"http://inl-labs.inl.nl/succeed/text?input=${java.net.URLEncoder.encode(text)}&format=text&tagger=bab-tagger&output=raw"

   def main(args:Array[String]):Unit =
    {
      val tagged = tag("""" Ghy syt een eesel, die syn sinnen hevet verloeren. Swyght!
        In den beginne schiep Godt den Hemel, ende de Aerde.
 De Aerde nu was woest ende ledich, ende duysternisse was op den afgront: ende de Geest Godts sweefde op de Wateren.
 Ende Godt seyde: Daer zy Licht: ende daer wert Licht.
Ende Godt sach het Licht, dat het goet was: ende Godt maeckte scheydinge tusschen het Licht, ende tusschen de duysternisse.
        """ )
        val c = Concordance(0,0,tagged,Map.empty)
      println(c.vertical)
    }
}

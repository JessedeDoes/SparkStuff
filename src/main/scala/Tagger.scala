import scala.xml._

object sidedish
{
  def getTextButNotIn(x:Node,tagName:String):String =
   {
       if (x.isInstanceOf[Text])
         x.text
       else 
         x.child.filter(_.label != tagName).map(getTextButNotIn(_,tagName)).mkString("")
   }
}

trait Tagger 
{
  
   import sidedish._
   
   def urlFor(text:String):String
   
   def taggedDocument(text:String):Elem = XML.load(urlFor(text))
   
   def tag(text:String):Map[String,Array[String]] =
   {
     val d = taggedDocument(text)
     val z = (d \\ "w").map(w => 
       List(
           ("word", getTextButNotIn(w,"interp")), 
           ("lemma", (w \ "@lemma").text), 
           ("pos", (w \ "@type").text)).toMap)
           
       val properties = z.flatMap(x => x.keys).toSet.toList
       val l = properties.map( (p:String) => (p -> z.map(m => m(p)).toArray)).toMap
       l  
   }
} 

object babTagger extends Tagger
{
   override def urlFor(text:String) = s"http://inl-labs.inl.nl/succeed/text?input=${text}&format=text&tagger=bab-tagger&output=raw" 
   
   def main(args:Array[String]):Unit = 
    {
      val tagged = tag("Ghy syt een eesel, die syn sinnen hevet verloeren. Swyght!")
      tagged.foreach({ case (p,a) => println(p + " -> " + a.toList.mkString(" "))})
    }
}

object chnTagger extends Tagger
{
   override def urlFor(text:String) = s"http://openconvert.clarin.inl.nl/openconvert/text?input=${text}&format=text&tagger=chn-tagger&output=raw&to=tei" 
   
    def main(args:Array[String]):Unit = 
    {
      val tagged = tag("Je ben een ezel die zijn verstand kwijt is. Zwijg!")
      tagged.foreach({ case (p,a) => println(p + " -> " + a.toList.mkString(" "))})
    }
}
   
import scala.xml._

trait Tagger 
{
  
} 

object babTagger extends Tagger
{
   def urlFor(text:String) = s"http://inl-labs.inl.nl/succeed/text?input=${text}&format=text&tagger=bab-tagger&output=raw" 
   def taggedDocument(text:String):Elem = XML.load(urlFor(text))
   
   def getTextButNotIn(x:Node,tagName:String):String =
   {
       if (x.isInstanceOf[Text])
         x.text
       else 
         x.child.filter(y => y.label != tagName).map(n => getTextButNotIn(n,tagName)).mkString("")
   }
   
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
   
   def main(args:Array[String]):Unit = (tag("Ghy syt een esel die syn sinnen hevet verlooren")).foreach(println)
}
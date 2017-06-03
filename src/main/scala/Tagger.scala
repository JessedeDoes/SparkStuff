import scala.xml._

trait Tagger 
{
  
} 

object babTagger extends Tagger
{
   def urlFor(text:String) = s"http://inl-labs.inl.nl/succeed/text?input=${text}&format=text&tagger=bab-tagger&output=raw" 
   def taggedDocument(text:String):Elem = XML.load(urlFor(text))
   
   def tag(text:String) =
   {
     val d = taggedDocument(text)
     val z = (d \\ "w").map(w => 
       List(
           ("word", w.text), 
           ("lemma", (w \ "@lemma").text), 
           ("pos", (w \ "@type").text)).toMap)
       
       
       val properties = z.flatMap(x => x.keys).toSet.toList
       val l = properties.map((p:String) => (p -> z.map(m => m(p)))).toMap
       l  
       // .map((p:String) => z.map(m => m(p)))
       // z.map(p => z.map 
       // map(p => (0 to z.size-1).map(i => z(i)
   }
   
   def main(args:Array[String]):Unit = (tag("Ghy syt een esel die syn sinnen hevet verlooren")).foreach(println)
}
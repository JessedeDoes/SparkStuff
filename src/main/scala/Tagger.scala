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
           ("pos", (w \ "@pos").text)).toMap)
       
       
       val a = z.flatMap(x => x.keys).toSet
       // map(p => (0 to z.size-1).map(i => z(i)
   }
   def main(args:Array[String]):Unit = println(taggedDocument("Ghy sy een esel die syn sinnen hevet verlooren"))
}
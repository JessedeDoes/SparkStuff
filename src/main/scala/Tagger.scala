import scala.xml._

trait Tagger 
{
  
} 

object babTagger extends Tagger
{
   def urlFor(text:String) = s"http://inl-labs.inl.nl/succeed/text?input=${text}&format=text&tagger=bab-tagger&output=raw" 
   def tag(text:String):Elem = XML.load(urlFor(text))
   
   def main(args:Array[String]):Unit = println(tag("Ghy sy een esel die syn sinnen hevet verlooren"))
}
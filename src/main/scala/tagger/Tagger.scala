package tagger

import scala.xml.{Elem, XML}

trait Tagger
{

   import sidedish._

   def urlFor(text:String):String

   def taggedDocument(text:String):Elem = XML.load(urlFor(text))

   def tag(text:String):Map[String,Array[String]] =
   {
     println("Text length: " + text.length)
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

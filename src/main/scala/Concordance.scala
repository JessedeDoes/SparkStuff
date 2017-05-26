

class Concordance(hitS: Int, hitE: Int, tokenProp:  Map[String,Array[String]], meta: Map[String,String])
{
  val hitStart = hitS
  val hitEnd = hitE
  val tokenProperties: Map[String,Array[String]] = tokenProp
  val metadata: Map[String,String] = meta
  val defaultProperty = "word"
  
  lazy val words = tokenProperties(defaultProperty)
  lazy val left = words.slice(0,hitStart).mkString(" ")
  lazy val right = words.slice(hitEnd, words.length).mkString(" ")
  lazy val hit = words.slice(hitStart, hitEnd).mkString(" ")
  
  
  override def toString() = (f"${left}%80s") + " "  + hit + "\t" + right
}
import scala.util.matching._
case class Filter(property: String, filter: String)

object Contextants
{
  def context(c: Concordance, w:Int, s:String):Seq[String] = 
   { 
     val a = c.tokenProperties(s)
     val L = a.length
     
     a.slice(Math.max(0,c.hitStart-w),c.hitStart).toList ++ a.slice( Math.min(L,c.hitEnd) , Math.min(L,c.hitEnd+w) ).toList
   }
  
   def context(c: Concordance, w:Int, s:String, f:Filter):Seq[String] = 
   { 
     val a = c.tokenProperties(s)
     val b = c.tokenProperties(f.property)
     
     val L = a.length
     val indexes = (Math.max(0,c.hitStart-w) to c.hitStart)   ++ (Math.min(L,c.hitEnd) to Math.min(L,c.hitEnd+w))
     val filtered = indexes.filter(k => b(k).matches(f.filter))
     return filtered.map(k => a(k))
   }
   
   
   def streamContext(c: => Stream[Concordance]):Stream[String] = 
   {
     c.flatMap(c => context(c,20,"lemma"))
   }
   
   def streamContext(c: => Stream[Concordance], f:Filter):Stream[String] = 
   {
     c.flatMap(c => context(c,20,"lemma",f))
   }
   def streamContext(c: => Stream[Concordance], w:Int, p:String, f:Filter):Stream[String] = 
   {
     c.flatMap(c => context(c,w,p,f))
   }
   
   def contextFrequencies(c: => Stream[Concordance], w:Int, p:String, f:Filter) = 
     streamContext(c,w,p,f).groupBy(s => s).map({ case (k,str) => (k, str.foldLeft(0)( (c,s) => c+1)) }).toList
   
   def contextFrequencies(c: => Stream[Concordance], f:Filter) = 
     streamContext(c,f).groupBy(s => s).map({ case (k,str) => (k, str.foldLeft(0)( (c,s) => c+1)) }).toList
}

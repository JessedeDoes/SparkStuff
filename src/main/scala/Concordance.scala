

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

object Contextants
{
  def context(c: Concordance, w:Int, s:String):List[String] = 
   { 
     val a = c.tokenProperties(s)
     val L = a.length
     
     a.slice(Math.max(0,c.hitStart-w),c.hitStart).toList ++ a.slice( Math.min(L,c.hitEnd) , Math.min(L,c.hitEnd+w) ).toList
   }
  
   def streamContext(c: => Stream[Concordance]):Stream[String] = 
   {
     c.flatMap(c => context(c,3,"word"))
   }
   
   def streamContext(c: => Stream[Concordance], w:Int, p:String):Stream[String] = 
   {
     c.flatMap(c => context(c,w,p))
   }
   
   def contextFrequencies(c: => Stream[Concordance], w:Int, p:String) = 
     streamContext(c,w,p).groupBy(s => s).map({ case (k,str) => (k, str.foldLeft(0)( (c,s) => c+1)) }).toList
   
   def contextFrequencies(c: => Stream[Concordance]) = 
     streamContext(c).groupBy(s => s).map({ case (k,str) => (k, str.foldLeft(0)( (c,s) => c+1)) }).toList
}



class Concordance(hitS: Int, hitE: Int, tokenProp:  Map[String,Array[String]], meta: Map[String,String])
{
  val hitStart = hitS
  val hitEnd = hitE
  val tokenProperties: Map[String,Array[String]] = tokenProp
  val metadata: Map[String,String] = meta
}

object Contextants
{
  def context(c: Concordance, w:Int, s:String):List[String] = 
   { 
     val a = c.tokenProperties(s)
     val L = a.length
     
     a.slice(Math.max(0,c.hitStart-w),c.hitStart).toList ++ a.slice( Math.min(L,c.hitEnd+1) , Math.min(L,c.hitEnd+w+1) ).toList
   }
}

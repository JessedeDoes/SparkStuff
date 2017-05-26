

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
  val defaultWindow = 30
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
     val indexes = (Math.max(0,c.hitStart-w) to c.hitStart-1)   ++ (Math.min(L,c.hitEnd) to Math.min(L,c.hitEnd+w-1))
     val filtered = indexes.filter(k => k > 0  && k < b.length && b(k).matches(f.filter))
     //println(filtered.toList + " " + a.toList)
     return filtered.map(k => a(k))
   }
   
  
   def streamContext(c: => Stream[Concordance], w:Int, p:String, f:Filter):Stream[String] = 
   {
     c.flatMap(c => context(c,w,p,f))
   }
   
   def contextFrequencies(c: => Stream[Concordance], w:Int, p:String, f:Filter) = 
     streamContext(c,w,p,f).groupBy(s => s).map({ case (k,str) => (k, str.foldLeft(0)( (c,s) => c+1)) }).toList
   
   def contextFrequencies(c: => Stream[Concordance], f:Filter): List[(String,Int)] = contextFrequencies(c, defaultWindow,"lemma",f)
   
   

    def logLikelyhood(freq:Int, freq1:Int, freq2:Int, totalBigrams:Int):Double = 
        {
                val n11:Double = freq;       // pair freq
                val n1p:Double = freq1;      // single freq of first word
                val np1:Double = freq2;      // single freq of second word
                val n12:Double = n1p - n11;
                val n21:Double = np1 - n11;
                val np2:Double = totalBigrams - 1 - np1;
                val n2p:Double = totalBigrams - 1 - n1p;
                val n22:Double = np2 - n12;
                val npp:Double = totalBigrams - 1;

                val m11:Double = n1p * np1 / npp;
                val m12:Double = n1p * np2 / npp;
                val m21:Double = n2p * np1 / npp;
                val m22:Double = n2p * np2 / npp;

                var logLikelihood = 0.0;

                if ( n11  != 0) { logLikelihood += n11 * StrictMath.log ( n11 / m11 ); }
                if ( n12  != 0) { logLikelihood += n12 * StrictMath.log ( n12 / m12 ); }
                if ( n21  != 0) { logLikelihood += n21 * StrictMath.log ( n21 / m21 ); }
                if ( n22  != 0 ) { logLikelihood += n22 * StrictMath.log ( n22 / m22 ); }

                return ( 2 * logLikelihood );
        }
   
   def salience(freq:Int, freq1:Int, freq2:Int, totalBigrams:Int):Double = 
        {
                var temp = ( freq / freq1.asInstanceOf[Double] ) /  freq2.asInstanceOf[Double];
                temp *= totalBigrams;
                return ( StrictMath.log(freq) * StrictMath.log(temp) / StrictMath.log(2.0) );
        }
   def dice(f:Int, f1:Int, f2:Int, N:Long):Double = 
        {
                return (2 * f / (f1 + f2).asInstanceOf[Double]);
        }
   def windowed[T](n:Int, s: Stream[T]):Stream[List[T]] = 
	 {
    if (n==0) 
      s match 
      { 
         case h #::t =>   List.empty  #:: Stream.empty 
         case _ => Stream.empty
      } 
    else s match 
      {  
        case h #::t=> (h :: windowed(n-1,t).head) #:: windowed(n,t); case _ => Stream.empty 
      }
	 }
}

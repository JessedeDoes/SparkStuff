import scala.util.matching._
case class Filter(property: String, filter: String)

object Collocation
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

                 2 * List( (n11,m11), (n12,m12), (n21,m21),(n22,m22) ).map( { case (n,m) => if (n != 0 && m != 0) n/m else 0 }).sum  
        }
   
   def salience(freq:Int, freq1:Int, freq2:Int, totalBigrams:Int):Double = 
        {
                val temp = ( freq / freq1.asInstanceOf[Double] ) /  freq2.asInstanceOf[Double];
              
                StrictMath.log(freq) * StrictMath.log(totalBigrams * temp) / StrictMath.log(2.0) 
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
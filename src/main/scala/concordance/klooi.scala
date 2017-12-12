package concordance

object klooi
{
  val s = List("doe","mij","danmaar","worst")
  
  def lengths(s:Seq[String]) =
  {
   
    def nextLength(s:Seq[String]) = (s.head.length,s.tail)
    
    lazy val lengths:Stream[(Int,Seq[String])] = (0 ,s)  #::
       lengths.map( 
           { 
             case (l,ss) => 
               if (ss == Seq.empty)
                 (l,Seq.empty)
               else
               {
                 val (ls,ns) = nextLength(ss); (l+ls, ns) 
               }
           }) 
    lengths.takeWhile(_._2 != Stream.empty).map( { case (l,s) => (l+s.head.length, s.head) } )
  }

  def withOffsets(s:Seq[String]):Seq[(String,Int,Int)] =
  {
    lengths(s).map( { case (i,w) => (w,i-w.length,i)})
  }

  def withOffsetsAndIndexDiscountedForExtraBlank(sentence:Seq[String]) =
  {
    withOffsets(sentence).zipWithIndex.map( { case ((w,s,e),i) => ((w,s+i,e+i),i)})
  }

  def main(args:Array[String]):Unit =
  {
    withOffsetsAndIndexDiscountedForExtraBlank(s).foreach(println)
  }
}
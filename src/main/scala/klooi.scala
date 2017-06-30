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

  def withOffsets(s:Seq[String]) =
  {
    lengths(s).map( { case (i,w) => (w,i-w.length,i)})
  }
  
  def main(args:Array[String]):Unit =
  {
    withOffsets(s).zipWithIndex.foreach(println)
  }
}
object klooi
{
  val s = List("doe","mij","danmaar","worst").toStream
  
  def lengths(s:Stream[String]) =
  {
   
    def nextLength(s:Stream[String]) = (s.head.length,s.tail)
    
    lazy val lengths:Stream[(Int,Stream[String])] = (0 ,s)  #:: 
       lengths.map( 
           { 
             case (l,ss) => 
               if (ss == Stream.empty)
                 (l,Stream.empty)
               else
               {
                 val (ls,ns) = nextLength(ss); (l+ls, ns) 
               }
           }) 
    lengths.takeWhile(_._2 != Stream.empty).map( { case (l,s) => (l+s.head.length, s.head) } )
  }
         
  
  def main(args:Array[String]) =
  {
    lengths(s).zipWithIndex.foreach(println)
  }
}
import scala.xml._


object dictionaryStructure
{ 
  case class Quotation(text:String, metadata:Map[String,String])
  
   def getQuotations(fileName: String):Seq[(String,String)]  = 
   {
     val entries = XML.load(fileName) \\ "entry"
     
     def doSense(sense:Node, path:List[String]):Seq[(String,String)] =
     {
       val id = sense \ "@id"
       val n = sense \ "@n"
       
       val newPath:List[String] = path ++ List(id + ":" + n) 
       val ps = newPath.mkString("/")
       
       val l0 = for (q <- (sense \ "eg" \ "cit" \ "q") ++ (sense \ "eg" \  "q"))
         yield (ps, q.text.replaceAll("\\s+", " "))
         
       val l1 = (sense \ "sense").flatMap(x => doSense(x.asInstanceOf[Elem],newPath))
       l0 ++ l1
     }
     
     (entries \ "sense").flatMap(s => doSense(s, List.empty))
   }
   
   def main(args:Array[String]):Unit = 
   {
     for (q <- getQuotations("/media/jesse/Data/Diamant/SERPENS/wolf.xml"))
         println(q)
   }
}
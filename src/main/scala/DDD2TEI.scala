import scala.xml._
import java.io._
object printMetadata
{
  def getMetadata(x: Node):Map[String,String] =
	 {
	   val m:Elem = (x \\ "recordData").head.asInstanceOf[Elem]
	   m.child.map(x => (x.label, x.text)).toMap
	 }
	 
	 def getMetadata(fileName:String): Map[String,String] = getMetadata(XML.load(fileName))
	 
	 def getMetadataForAll(dir: String):Stream[Map[String,String]] =
	   new File(dir).list.toStream.map(f => getMetadata(dir + "/" + f))
	 
	 def printAllMetadata(dir:String) = 
	   getMetadataForAll(dir).map(m => m.toList.map({ case (k,v) => k  + ":" + v } ).mkString("\t")).foreach(println)
	   
	 def main(args:Array[String]):Unit = printAllMetadata(args(0))
}
object toTEI
{
   import scala.util.matching._
   val Date = new Regex("^\\s*([0-9]{4})[^0-9]([0-9]{1,2})[^0-9]([0-9]{1,2}).*")
   
   def dateField(name:String, value:String): Elem =
     <interpGrp type={name}>
				<interp value={value}/>
		</interpGrp>
   
	 
	 
   def makeTEI(metadataRecord: Elem, text: Elem, id:String):Elem =
   {
     val cleanerId = id.replaceAll(".*urn=","")
     
     
     val Date(year,month,day) = (metadataRecord \ "date")(0).text
     val l = List( ("Year_",year), ("Month_",month),("Day_",day))
     val sillyXML = l.map( { case (n,v) =>  List("witness","text","pub").map(t => List("from","to").map(ft => dateField(t + n + ft, v))) })
   
     val title = (text \\ "title")(0).text
   
     val interpjes = (metadataRecord.child).map(x => <interpGrp type={x.label}><interp value={x.text}/></interpGrp>)
     val papertitle = if ((metadataRecord \ "paperTitle").size > 0) (metadataRecord \ "paperTitle")(0).text else ""
   
     val textContent = (text \ "text")(0).child
<TEI>
<teiHeader>
<fileDesc><titleStmt><title>{title}</title>
</titleStmt><publicationStmt><p/></publicationStmt>
<sourceDesc><p>whatever</p>
<listBibl id="inlMetadata">
<bibl>
{sillyXML}
<interpGrp type="titleLevel1">
  <interp value={title}/>
</interpGrp>
<interpGrp type="titleLevel2">
  <interp value={papertitle}/>
</interpGrp>
<interpGrp type="idno">
  <interp value={cleanerId}/>
</interpGrp>
{interpjes}
</bibl>
</listBibl>
</sourceDesc>
</fileDesc>
</teiHeader>
<text>
<body>
{textContent}
</body>
</text>
</TEI>
   }
   
   val xmlFilter = new java.io.FilenameFilter { def accept(dir:File, name:String):Boolean =  name.toLowerCase.endsWith(".xml") }
  
   def save(dir:String,doc:Elem,fileName:String):Unit = { new PrintWriter(dir +"/" + fileName) { write(doc.toString()); close } }
   
   def convertToTEI(fromDir:String, toDir:String) =
   {
     val f = new File(fromDir)
     if (f.isDirectory)
     {
       val str = f.listFiles(xmlFilter).toStream
       val z = for {
             fi <- str
             n = fi.getName
             x = XML.load(fromDir + "/" + n) 
             id = (x \\ "identifier").text
             meta = (x \\ "recordData")(0).asInstanceOf[Elem]
             tei = makeTEI(meta, x, id)
             } 
        yield 
         save(toDir, tei, n.replaceAll("xml", "tei.xml")) 
        z.foreach( Nil=>Nil ); 
     }
   }
   
   def main(args:Array[String]):Unit =
   {
     convertToTEI(args(0), args(1))
   }
}
import scala.xml._

// http://jsru.kb.nl/sru/sru?operation=searchRetrieve&x-collection=ANP&query=date+within+%2201-01-1960+01-01-1961%22+AND+Zeitung

object Download
{
  val base = "http://jsru.kb.nl/sru/sru?operation=searchRetrieve&x-collection=DDD_krantnr"
  def get(url: String) = scala.io.Source.fromURL(url).mkString
  
  val p = 100
  
   
  val SRU_base = "http://jsru.kb.nl/sru/sru" + "?version=1.2&operation=searchRetrieve" + 
  "&x-collection=DDD_artikel&recordSchema=ddd" + 
  "&startRecord=0&maximumRecords=" + p + "&query=date+within+%22"
  
  def getIdentifiers(base: String, startDate:String, endDate:String, term:String):Seq[String] =
  {
    val url = s"$base$startDate+$endDate%22+AND+$term"
    val n = math.min(getNumberOfResults(url),300)
    
    (0 to n by p).flatMap(k => getIdentifiers(url,k))
  }
  
  def getNumberOfResults(url:String):Int =
  {
    val xml = XML.load(url.replaceAll("maximumRecords=[0-9]+","maximumRecords=1"))
    (xml \\ "numberOfRecords").text.toInt
  }
  
  def getIdentifiers(url: String, start:Int):Seq[String] =
  {
    val u1 = url.replaceAll("startRecord=0","startRecord="+start)
    Console.err.println("loading:" + u1)
    val xml = XML.load(u1)
   
    (xml \\ "identifier").map(x => x.text)
  }
  
  def main(args: Array[String]) =
  {  
    
    for (id <- getIdentifiers(SRU_base, "20-01-1820","30-01-1820","Napoleon"))
    {
      println(id)
      try 
      {
        println(get(id))
      } catch   
      {
        case e:Exception => Console.err.println("nou hoor...")
      }
    }
  }
  
  val exampleRecord = <srw:record>
<srw:recordPacking>xml</srw:recordPacking>
<srw:recordSchema>http://www.kb.nl/ddd</srw:recordSchema>
<srw:recordData>
<dc:identifier>
http://resolver.kb.nl/resolve?urn=ddd:010599630:mpeg21:a0156:ocr
</dc:identifier>
<ddd:metadataKey>
http://resolver.kb.nl/resolve?urn=ddd:010599630:mpeg21:a0156
</ddd:metadataKey>
<dc:type>illustratie met onderschrift</dc:type>
<ddd:spatial>Regionaal/lokaal</ddd:spatial>
<ddd:page>20</ddd:page>
<ddd:edition>Dag</ddd:edition>
<ddd:forerunner>
Journal du département de la Frise = Dagblad van het departement Vriesland
</ddd:forerunner>
<ddd:pageurl>ddd:010599630:mpeg21:p020</ddd:pageurl>
<ddd:paperurl>
http://resolver.kb.nl/resolve?urn=ddd:010599630:mpeg21
</ddd:paperurl>
<ddd:yearsdigitized>1813-1942</ddd:yearsdigitized>
<ddd:publisher>D.R. Smeding en M. Koon</ddd:publisher>
<dc:source>KBDK</dc:source>
<ddd:ppn>852115210</ddd:ppn>
<ddd:accessible>1</ddd:accessible>
<ddd:papertitle>Leeuwarder courant</ddd:papertitle>
<dc:date>1912/12/30 00:00:00</dc:date>
<ddd:issued>1813-1942</ddd:issued>
<ddd:spatialCreation>Leeuwarden</ddd:spatialCreation>
<dc:title>DOMME HONDJES.</dc:title>
</srw:recordData>
<srw:recordPosition>1</srw:recordPosition>
</srw:record>
}
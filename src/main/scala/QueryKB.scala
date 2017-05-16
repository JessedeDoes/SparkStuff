import scala.xml._

case class ContentQuery(startDate:String, 
    endDate:String, textQuery:String)
    
case class SRUQuery(server:String, 
    operation:String, collection:String, 
    startRecord:Int, maximumRecords:Int, query:ContentQuery)



object Download
{
  val base = "http://jsru.kb.nl/sru/sru?operation=searchRetrieve&x-collection=DDD_krantnr"

  
  val batchSize = 100
  val maxDocuments = 1000
  val defaultStartDate = "01-01-1800"
  val defaultEndDate = "31-01-1939"
  val defaultCollection = "DDD_artikel"
       
  val SRU_base = "http://jsru.kb.nl/sru/sru" + "?version=1.2&operation=searchRetrieve" + 
  "&x-collection=DDD_artikel&recordSchema=ddd" + 
  "&startRecord=0&maximumRecords=" + batchSize + "&query=date+within+%22"
  
  val beesten = List("Adder", "Bever", "Beverrat", "Boommarter", "Bunzing", "Das", 
      "Dennensnuitkever", "Fret", "Hermelijn", "Huismuis", "Konijn", "Lynx", "Muskusrat", 
      "Otter", "Raaf", "Spreeuw", "Vos", "Wezel", "Wolf")
  
  def mkString(q: ContentQuery) ="date+within+%22" + q.startDate + "+" + q.endDate + "%22+AND+" + q.textQuery
  
  def mkURL(q: SRUQuery): String =
  q.server + "&operation=" + q.operation + "&x-collection=" + q.collection + "&startRecord=" + 
  q.startRecord + "&maximumRecords=" + q.maximumRecords + "&query=" + mkString(q.query)
  
  def query(term:String):SRUQuery = SRUQuery("http://jsru.kb.nl/sru/sru?version=1.2", "searchRetrieve", 
      defaultCollection, 0, maxDocuments, ContentQuery(defaultStartDate, defaultEndDate, term))
      
  def get(url: String) = scala.io.Source.fromURL(url).mkString
  
  def getNumberOfResults(q:SRUQuery):Int = 
  {
    val q0 = q.copy(startRecord=0,maximumRecords=1)
    val url = mkURL(q0)
    Console.err.println(url)
    getNumberOfResults(url)
  }
  
  def getNumberOfResults(url:String):Int =
  {
    val xml = XML.load(url.replaceAll("maximumRecords=[0-9]+","maximumRecords=1"))
    val n = (xml \\ "numberOfRecords").text.toInt
    Console.err.println("number of documents:" + n)
    n
  }
  
  
  
  def matchingDocumentIdentifiers(q:SRUQuery):Seq[(String,Node)] =
  {
    //val url = 
    val n = math.min(getNumberOfResults(q),maxDocuments)
    
    (0 to n by batchSize).flatMap(k => getMatchingDocumentIdentifiersForBatch(q,k,batchSize))
  }
  

  def getMatchingDocumentIdentifiersForBatch(q:SRUQuery, start:Int, maximum:Int):Seq[(String,xml.Node)] =
  {
    val q1 = q.copy(startRecord=start, maximumRecords=maximum)
    val xml = XML.load(mkURL(q1))
    for  { 
      r <- xml \\ "recordData"; 
      id <- r \\ "identifier"}
      yield
        (id.text, r)
  }
   
  def main(args: Array[String]) =
  {  
    val aantallen = beesten.map(b => (b,getNumberOfResults(query(b)))) 
    
    println(aantallen)
    
    
    for ((id,meta) <- matchingDocumentIdentifiers(query("Bunzing")))
    {
      val metadata = (List("date", "papertitle", "title").map(x => (meta \\ x).text)).mkString("\t")
   
      println(id + "\t" + metadata)
      
      try 
      {
        println("document length:" + get(id).length())
      } catch   
      {
        case e:Exception => Console.err.println(s"nou hoor..., kan $id niet afhalen")
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
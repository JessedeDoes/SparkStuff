/*
 * Stuff for querying KB newspapers
 * 
 */
import scala.xml._

trait TextQuery
{
	override def toString() = this match
			{
			case Term(s) => s
			case And(t1,t2) => "(" + t1.toString + "+AND+" + t2.toString + ")"
			case Or(t1,t2) => "(" + t1.toString + "+OR+" + t2.toString + ")"
			case Phrase(l @ _*) => "%22" + l.map(_.toString).mkString("+")  + "%22"
			}
}

case class Term(term:String) extends TextQuery
case class And(t1:TextQuery, t2:TextQuery) extends TextQuery
case class Or(t1:TextQuery, t2:TextQuery) extends TextQuery
case class Phrase(l: TextQuery*) extends TextQuery

trait ContentQueryT
{
	def startDate:String
	val endDate:String
	val textQuery:TextQuery
	def toParameterValue():String ="date+within+%22" + startDate + "+" + endDate + "%22+AND+" + textQuery
}

trait SRUQueryT
{
	def server:String 
	def operation:String
	def collection:String 
	def startRecord:Int 
	def maximumRecords:Int
	def query:ContentQuery
	def mkURL(): String =
	server + "&operation=" + operation + "&x-collection=" + collection + "&startRecord=" + 
			startRecord + "&maximumRecords=" + maximumRecords + "&query=" + query.toParameterValue()
}

case class ContentQuery(startDate:String, 
    endDate:String, textQuery:TextQuery) extends ContentQueryT
    
case class SRUQuery(server:String, 
    operation:String, collection:String, 
    startRecord:Int, maximumRecords:Int, query:ContentQuery) extends SRUQueryT
    
object Download
{
  val base = "http://jsru.kb.nl/sru/sru?operation=searchRetrieve&x-collection=DDD_krantnr"

  
  val batchSize = 100
  val maxDocuments = 1000
  val defaultStartDate = "01-01-1800"
  val defaultEndDate = "31-01-1939"
  val defaultCollection = "DDD_artikel"
  val defaultServer = "http://jsru.kb.nl/sru/sru?version=1.2"
         
  val beesten = List("Adder", "Bever", "Beverrat", "Boommarter", "Bunzing", "Das", 
      "Dennensnuitkever", "Fret", "Hermelijn", "Huismuis", "Konijn", "Lynx", "Muskusrat", 
      "Otter", "Raaf", "Spreeuw", "Vos", "Wezel", "Wolf")
  
  def wrapTextQuery(t:TextQuery) = SRUQuery(defaultServer, "searchRetrieve", 
             defaultCollection, 0, maxDocuments, 
             ContentQuery(defaultStartDate, defaultEndDate, t))
             
  def singleWordQuery(term:String):SRUQuery = wrapTextQuery(Term(term))
  
  def get(url: String) = scala.io.Source.fromURL(url).mkString
  
  def getNumberOfResults(q:SRUQuery):Int = 
  {
    val q0 = q.copy(startRecord=0,maximumRecords=1)
    val url = q0.mkURL()
    Console.err.println(url)
    val n = getNumberOfResults(url)
    Console.err.println("number of matching documents:" + n + " for " + q.query)
    n
  }
  
  def getNumberOfResults(url:String):Int =
  {
    val xml = XML.load(url)
    val n = (xml \\ "numberOfRecords").text.toInt
    n
  }
  
  

  
  /*
   * There might be millions, so we do not want to keep the metadata record XML nodes in memory all at once,
   * so we to return need a stream instead of a List
   */
  def matchingDocumentIdentifiers(q:SRUQuery):Stream[(String,Node)] =
  {
		  def getMatchingDocumentIdentifiersForBatch(q:SRUQuery, start:Int, maximum:Int):Stream[(String,xml.Node)] =
			  {
					  Console.err.println("Start batch at " + start)
					  val q1 = q.copy(startRecord=start, maximumRecords=maximum)
					  val xml = XML.load(q1.mkURL)
					  for  { 
						  r <- (xml \\ "recordData").toStream; 
						  id <- r \\ "identifier"}
					  yield
					  (id.text, r)
			  }
    
    val n = math.min(getNumberOfResults(q),maxDocuments)
    
    val x = (0 to n by batchSize).toStream.flatMap(start => getMatchingDocumentIdentifiersForBatch(q,start,batchSize))
    x
  }
  


  implicit def StringToTerm(s:String):Term = Term(s)
  
  def main(args: Array[String]) =
  {  
    val aantallen = beesten.map(b => (b,getNumberOfResults(singleWordQuery(b)))) 
    
    println(aantallen)
    
    val n = this.getNumberOfResults(wrapTextQuery(Phrase("de", "kool", "en", "de", "geit")))
    
    for ((id,metadataRecord) <- matchingDocumentIdentifiers(singleWordQuery("Bunzing")))
    {
      val basicMeta = (List("date", "papertitle", "title").map(x => (metadataRecord \\ x).text)).mkString("\t")
   
      println(id + "\t" + basicMeta)
      
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
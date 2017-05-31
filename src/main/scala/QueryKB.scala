/*
 * Stuff for querying KB newspapers
 * 
 * Problem: why is paperTitle missing
 * 
 */
import scala.xml._
import java.io.PrintWriter;
import java.io.File



    
object Store
{
  val dir = "./Store"
  def store(subdir: String, id:String, metadata:Node, text:String) = 
  {
    val fileName = id.replaceAll(".*urn=","").replaceAll(":","_") + ".xml"
    val xml = XML.loadString(text)
    val doc = <doc>{metadata}{xml}</doc>
    val f = new File(dir + "/" + subdir)
    if (!f.isDirectory()) f.mkdir()
    new PrintWriter(dir + "/" + subdir + '/' + fileName) { write(doc.toString()); close }
  }
   def exists(subdir: String, id:String):Boolean = 
   {
     val fileName = id.replaceAll(".*urn=","").replaceAll(":","_") + ".xml"
     val oPath = dir + "/" + subdir + '/' + fileName
     return new File(oPath).exists
   }
}



object QueryKB
{
  val base = "http://jsru.kb.nl/sru/sru?operation=searchRetrieve&x-collection=DDD_krantnr"

  
  val batchSize = 100
  val maxDocuments = Int.MaxValue
  //val defaultStartDate = "01-01-1800"
  //val defaultEndDate = "31-01-1939"
  val defaultStartDate = "01-01-1700"
  val defaultEndDate = "31-01-1799"
  val defaultCollection = "DDD_artikel"
  val defaultServer = "http://jsru.kb.nl/sru/sru?version=1.2"
         
  val beesten = List("Adder", "Bever", "Beverrat", "Boommarter", "Bunzing", "Das", 
      "Dennensnuitkever", "Fret", "Hermelijn", "Huismuis", "Konijn", "Lynx", "Muskusrat", 
      "Otter", "Raaf", "Spreeuw", "Vos", "Wezel", "Wolf")
  
  def wrapTextQuery(t:TextQuery) = 
          SRUQuery(defaultServer, "searchRetrieve", 
             defaultCollection, 0, maxDocuments, 
             ContentQuery(defaultStartDate, defaultEndDate, t))
             
  def singleWordQuery(term:String):SRUQuery = wrapTextQuery(SingleTerm(term))
  
  def expandedQuery(term:String):SRUQuery = 
  {
    val l = LexiconService.getWordforms(term)
    val l1 = if (l.contains(term.toLowerCase)) l else term.toLowerCase :: l
    wrapTextQuery(ListDisjunction(l1.map( x => SingleTerm(x))))
  }
  
  def get(url: String) = scala.io.Source.fromURL(url).mkString
  
  def getNumberOfResults(q:SRUQuery):Int = 
  {
    val q0 = q.copy(startRecord=0,maximumRecords=1)
    val url = q0.mkURL()
    Console.err.println(url)
    val n = getNumberOfResultsFromURL(url)
    Console.err.println("number of matching documents:" + n + " for " + q.query)
    n
  }
  
  def getNumberOfResultsFromURL(url:String):Int =
  {
    val xml = XML.load(url)
    val n = (xml \\ "numberOfRecords").text.toInt
    n
  }
    
  /**
   * Return a list of pairs: first is article id (actually resolver URI), second is recordData containing metadata for the article
   * There might be millions, so we do not want to keep the metadata record XML nodes in memory all at once,
   * so we to return need a stream instead of a List
   */
  def matchingDocumentIdentifiers(q:SRUQuery):Stream[(String,Node)] =
  {
		  def getMatchingDocumentIdentifiersForBatch(q:SRUQuery, start:Int, maximum:Int):Stream[(String,xml.Node)] =
			  {
					  Console.err.println("Get metadata for batch starting at " + start)
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
  


  implicit def StringToTerm(s:String):SingleTerm = SingleTerm(s)
  implicit def StringToQuery(s:String):SRUQuery = singleWordQuery(s)
  
  
  def download(id:String,metadataRecord:Node, subdir:String) =     
  try 
      {
        if (!Store.exists(subdir,id))
        {
          val txt = get(id);
          Store.store(subdir,id, metadataRecord,txt)
          println(s"document length for $id:" + txt.length())
        }
      } catch   
      {
        case e:Exception => Console.err.println(s"nou hoor..., kan $id niet afhalen: " + e)
      }
    
  def downloadForTermList(l:List[String])  = l.par.map(downloadQueryResults)
   
         //b => matchingDocumentIdentifiers(singleWordQuery(b)).map({ case (i,m) => download(i,m,b) })
         
  def downloadQueryResults(s:String) =
    for ((id,metadataRecord) <- matchingDocumentIdentifiers(s))
      download(id,metadataRecord,s)
      
  def downloadPar(s:String)
  {
      val s0 = matchingDocumentIdentifiers(s)
      val split = splitStream(s0,5)
      split.par.foreach(
           x =>  
             for ((id,metadataRecord) <- x)
             { download(id,metadataRecord,s) }
      )
  }
      
 
  def test = 
  {
		  val aantallen = beesten.map(b => (b,getNumberOfResults(singleWordQuery(b)))) 
		  
			println(aantallen)

			val aantallen1 = beesten.map(b => (b,getNumberOfResults(expandedQuery(b)))) 
			println(aantallen1)
			
			System.exit(0)
			val n = this.getNumberOfResults(wrapTextQuery(Phrase("de", "kool", "en", "de", "geit")))

			for ((id,metadataRecord) <- matchingDocumentIdentifiers(singleWordQuery("Konijn")))
				{
					  val basicMeta = (List("date", "papertitle", "title").map(x => (metadataRecord \\ x).text)).mkString("\t")
						download(id,metadataRecord, "Test")
				}
  }
  

  def splitStream[A](seq: Iterable[A], n: Int) = 
  {
    (0 until n).map(i => seq.drop(i).sliding(1, n).flatten)
  }
  
  // round(Stream.from(1),3).foreach(i => println(i.take(3).toList))
  
}


object KBKwic
{
  import Tokenizer._
  import QueryKB._
  def window=8
  
  case class Kwic(left:String, hit:String, right:String)
  {
    override def toString():String = (f"${left}%80s") + "\t"  + hit + "\t" + right
  }
  
  def concordance(query:String,document:Node):List[Kwic] = 
  {
    val tokens = tokenize(document.text)
    // println(tokens.toList)
    val matchPositions = (0 to tokens.length-1).toList.filter(i => tokens(i).token.toLowerCase == query.toLowerCase())
    //println(matchPositions)
    def slice = (a:Int,b:Int) => tokens.slice(Math.max(0,a),Math.min(b,tokens.length-1)).toList.map(
        t => t.leading + t.token + t.trailing).mkString(" ")
    def getMatch(p:Int) = Kwic(slice(p-window,p), tokens(p).token, slice(p+1,p+window+1))
    matchPositions.map(getMatch)
  }
  
  def concordanceFile(query:String, fileName:String):List[Kwic] = concordance(query,XML.load(fileName))
  
  def concordancesDir(query:String, dirName:String):List[Kwic] =
    new File(dirName).list().toList.flatMap(f => concordanceFile(query, dirName + "/" + f))
  
  def concordanceDir(query:String, dirName:String):Unit = concordancesDir(query,dirName).foreach(println)
  
  def concordance(query:String, url:String):List[Kwic] = concordance(query,XML.load(url))
  
  def kwicResults(s:String) =
    for ((id,metadataRecord) <- matchingDocumentIdentifiers(s))
      println(concordance(s, id))
      
  def kwicResultsPar(s:String)
  {
      val s0 = matchingDocumentIdentifiers(s)
      val split = splitStream(s0,3)
      split.par.foreach(
           x =>  
             for ((id,metadataRecord) <- x)
             { println(KBKwic.concordance(s, id)) }
           
      )
  }
  def main(args:Array[String]) = if (args.length >= 2) concordanceDir(args(0),args(1)) else kwicResults(args(0))
}

object Download
{
	import QueryKB._
	def main_old(args: Array[String]):Unit =
  {
		downloadForTermList(beesten.filter(s => {val x:Int = getNumberOfResults(s); (x >  35000 && x < 200000) }))
  } 
	
	def main(args: Array[String]):Unit =
  {
    downloadPar("wolf")
    downloadPar("wolven")
  } 
  
}
object countKB
{
   import QueryKB._
   def main(args: Array[String]):Unit =
  {
     println(getNumberOfResults(args(0)))
  } 
}
object stuff
{
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

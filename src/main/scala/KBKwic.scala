import scala.xml._
import java.io._

object KBKwic
{
  import Tokenizer._
  import SRU._
  import QueryKB._
  def window=15
  

  def containsOneOf(haystack:String, needles: Set[String]):Boolean = needles.exists(n => haystack.toLowerCase.contains(n.toLowerCase()))  // map(n => haystack.toLowerCase.contains(n.toLowerCase()) ).reduce((a,b) => a || b)
  def concordance(query:TextQuery, text:String):List[Concordance] =
  {
    val tokens = tokenize(text)
    val terms = SRU.termsIn(query).map(_.toLowerCase)

    val matchPositions = (0 to tokens.length-1).toList.filter(i =>  containsOneOf(tokens(i).token, terms) )



    def sliceTokenProperties(a:Int,b:Int):Map[String, Array[String]] =
      {
        val tokz = tokens.slice(a,b)
        Map( "prepunctuation" -> tokz.map(_.leading),
             "word" -> tokz.map(_.token),
             "postpunctuation" -> tokz.map(_.trailing) )
      }

    def startFor(p:Int) = Math.max(0,p-window)
    def endFor(p:Int) = Math.min(p+window+1,tokens.length)
    def conc(p:Int)= Concordance(p-startFor(p), p-startFor(p)+1, sliceTokenProperties(startFor(p), endFor(p)) , Map.empty)

    matchPositions.map(conc)
  }

  def concordance(query:String,document:Node, meta:Node):List[Concordance] = concordance(SingleTerm(query), document, meta)

  def concordance(query:TextQuery, document:Node, meta: Node):List[Concordance] = concordance(query, document.text).map(c => c.copy(metadata=KBMetadata.getMetadata(meta)))

  def concordanceFile(query:String, fileName:String):List[Concordance] = { val d = XML.load(fileName); concordance(query, d , d) }
  
  def concordancesDir(query:String, dirName:String):List[Concordance] =
    new File(dirName).list().toList.par.flatMap(f => concordanceFile(query, dirName + "/" + f)).toList
  

  def concordanceURL(query:String, url:String, meta:Node):List[Concordance] = concordance(query,XML.load(url), meta)

  def concordanceURL(query:TextQuery, url:String, meta:Node):List[Concordance] = concordance(query,XML.load(url), meta)

  def kwicResults(s:String):Stream[Concordance] =
    matchingDocumentIdentifiers(s).flatMap( { case (id, metadataRecord) => concordanceURL(s, id, metadataRecord).toStream } )

  def concordanceDir(query:String, dirName:String):Unit = concordancesDir(query,dirName).foreach(println)

  def kwicResultsTagged(t:TextQuery)
  {
    val s0 = matchingDocumentIdentifiers(t)
    val split = splitStream(s0,100)
    split.par.foreach(
      x =>
        for ((id,metadataRecord) <- x)
        { println(KBKwic.concordanceURL(t, id, metadataRecord) .map( c => c.tag(chnTagger).vertical)) }
    )
  }

  def kwicResultsPar(t:TextQuery)
  {
      val s0 = matchingDocumentIdentifiers(t)
      val split = splitStream(s0,100)
      split.par.foreach(
           x =>  
             for ((id,metadataRecord) <- x)
             { println(KBKwic.concordanceURL(t, id, metadataRecord) /* .map( c => c.tag(chnTagger).vertical) */) }
      )
  }

  def kwicResultsParx(t:TextQuery):Stream[Concordance] =
  {
    val s0 = matchingDocumentIdentifiers(t)
    val split = splitStream(s0,100)
    split.par.flatMap(x =>
      (for ( (id,metadataRecord) <- x; c  <- KBKwic.concordanceURL(t, id, metadataRecord)) yield c).toStream
    ).toStream
  }
  
  def main(args:Array[String]):Unit =
    {
      val arg0 = if (args.length == 0) "bunzing" else args(0)
      if (args.length >= 2) concordanceDir(arg0,args(1)) else kwicResults(arg0).foreach(println)
    }
}
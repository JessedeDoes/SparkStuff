import scala.xml._
import java.io._

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
    new File(dirName).list().toList.par.flatMap(f => concordanceFile(query, dirName + "/" + f)).toList
  
  def concordanceDir(query:String, dirName:String):Unit = concordancesDir(query,dirName).foreach(println)
  
  def concordance(query:String, url:String):List[Kwic] = concordance(query,XML.load(url))
  
  def kwicResults(s:String) =
    for ((id,metadataRecord) <- matchingDocumentIdentifiers(s))
      println(concordance(s, id))
      
  def kwicResultsPar(s:String)
  {
      val s0 = matchingDocumentIdentifiers(s)
      val split = splitStream(s0,5)
      split.par.foreach(
           x =>  
             for ((id,metadataRecord) <- x)
             { println(KBKwic.concordance(s, id)) }
           
      )
  }
  def main(args:Array[String]) = if (args.length >= 2) concordanceDir(args(0),args(1)) else kwicResultsPar(args(0))
}
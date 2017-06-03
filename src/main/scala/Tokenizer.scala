import java.util.regex.Matcher
import java.util.regex.Pattern
import scala.collection.mutable.ArrayBuffer

import scala.io.Source

case class Token(leading:String, token:String, trailing:String)

trait Tokenizer
{
   def tokenize(s:String): Array[Token] 
}

trait EntityReplacer
{
   def substitute(s:String)(implicit mapping:Map[String,String]):String
}

object entities extends EntityReplacer
{
  implicit val entityFile = "src/main/resources/wntchars.tab"
  
  val entityPattern = """(&[^;\\s]*;)""".r
  val hexPattern = """&#x([0-9a-fA-F]{2,4});""".r
  val decimalPattern = """&#([0-9]{2,4});""".r
  
  def readMappingFile(implicit fileName:String):Map[String,String] =
    Source.fromFile(fileName).getLines.map(l => (l.split("\t")(0), l.split("\t")(1))).toMap
  
  // def toChar(s:String) =
    
  implicit lazy val defaultMapping = readMappingFile
  
  def replaceNumericEntities = ???
    
  override def substitute(s:String)(implicit mapping:Map[String,String]):String =
  {
    val replaceOne = (s:String) => if (mapping.contains(s)) mapping(s) else s
    val s1 = entityPattern.replaceAllIn(s, m => replaceOne(m.group(0)))
    val s2 = hexPattern.replaceAllIn(s1, m =>   (Integer.parseInt((m.group(1)),16)).toChar.toString)
    val s3  = decimalPattern.replaceAllIn(s2, m =>   (Integer.parseInt((m.group(1)),10)).toChar.toString)
    s3.replaceAll("<[^<>]*>","")
  }
  
  def main(args:Array[String]):Unit = 
  {
    println(substitute("Garsias &#x2026; was dit"))
  }
}

object Tokenizer extends Tokenizer
{
  import scala.util.matching._
  val Split = new Regex("^(\\p{P}*)(.*?)(\\p{P}*)$")
  
  
  
  def tokenizeOne(s:String): Token =
  {
     val Split(l,c,r) = s
     Token(l,c,r)
  }
  
  def doNotTokenize(s:String): Token = Token("",s,"")
  
  override def tokenize(s:String): Array[Token] = 
    s.split("\\s+").map(tokenizeOne)
   
  def main(args:Array[String]):Unit = 
  {
    println(tokenize("The dog, i think, is 'hardly-' interesting??!").toList);
  }
}

object TokenizerWithOffsets
{
  import Tokenizer._
  lazy val notWhite = Pattern.compile("\\S+")
  
  case class TokenWithOffsets(token:Token, startPosition:Int, endPosition:Int)
  
  implicit val tokenize = true
  def tokenize(s:String)(implicit really:Boolean): Array[TokenWithOffsets] =
  {
    val matcher = notWhite.matcher(s)
    val r = new ArrayBuffer[TokenWithOffsets]
    while (matcher.find)
    {
      val t = if (really) tokenizeOne(matcher.group) else doNotTokenize(matcher.group)
      val z = TokenWithOffsets(t, matcher.start,  matcher.end)
      r += z
    }
    r.toArray
  }
 
  def main(args:Array[String]):Unit = println(TokenizerWithOffsets.tokenize("Waarom, waarom, hebt u mij verlaten??").toList)
}
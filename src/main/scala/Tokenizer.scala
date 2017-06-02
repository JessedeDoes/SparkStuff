import java.util.regex.Matcher
import java.util.regex.Pattern
import scala.collection.mutable.ArrayBuffer
object Tokenizer
{
  import scala.util.matching._
  val Split = new Regex("^(\\p{P}*)(.*?)(\\p{P}*)$")
  case class Token(leading:String, token:String, trailing:String)
  
  
  def tokenizeOne(s:String): Token =
  {
     val Split(l,c,r) = s
     Token(l,c,r)
  }
  
  def tokenize(s:String): Array[Token] = 
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
  
  def tokenize(s:String): Array[TokenWithOffsets] =
  {
    val matcher = notWhite.matcher(s)
    val r = new ArrayBuffer[TokenWithOffsets]
    while (matcher.find)
    {
      val z = TokenWithOffsets(tokenizeOne(matcher.group), matcher.start,  matcher.end)
      r += z
    }
    r.toArray
  }
 
   def main(args:Array[String]):Unit = println(TokenizerWithOffsets.tokenize("Waarom, waarom, hebt u mij verlaten??").toList)
}
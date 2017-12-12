package tokenizer

import java.util.regex.Pattern

import scala.collection.mutable.ArrayBuffer











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
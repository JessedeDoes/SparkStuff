package tokenizer

trait Tokenizer
{
  case class Token(leading:String, token:String, trailing:String)

   def tokenize(s:String): Array[Token]
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
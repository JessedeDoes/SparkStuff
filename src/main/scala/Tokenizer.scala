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
    println(tokenize("The dog, i think, is 'hardly' interesting??!").toList);
  }
}
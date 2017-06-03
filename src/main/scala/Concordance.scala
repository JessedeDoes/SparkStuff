

case class Concordance(hitS: Int, hitE: Int, tokenProp:  Map[String,Array[String]], meta: Map[String,String])
{
  val hitStart = hitS
  val hitEnd = hitE
  val tokenProperties: Map[String,Array[String]] = tokenProp
  val metadata: Map[String,String] = meta
  val defaultProperty = "word"
  
  lazy val words = tokenProperties(defaultProperty)
  lazy val left = words.slice(0,hitStart).mkString(" ")
  lazy val right = words.slice(hitEnd, words.length).mkString(" ")
  lazy val hit = words.slice(hitStart, hitEnd).mkString(" ")
  
  def retokenize(t:Tokenizer):Concordance = ???
  
  def tag(implicit tagger:Any):Concordance = ???
  
  override def toString() = (f"${left}%80s") + " \u169b"  + hit + "\u169c " + right + " metadata: " + meta
 }




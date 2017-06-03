

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
  
  def retokenize(t:Tokenizer) = 
  {
    val tokens = tokenProperties("word").map(s => t.tokenize(s).head)
    val prepunct = tokens.map(t => t.leading)
    val postpunct = tokens.map(t => t.trailing)
    val words = tokens.map(t => t.token)
    val newProperties = tokenProperties -- List("words","prepunctuation","postpunctuation") ++ List("word" -> words, "prepunctuation" -> prepunct, "postpunctuation" -> postpunct)
    this.copy(tokenProp=newProperties)
  }
  
  def vertical =
  {
    val keys = tokenProperties.keys
    (0 to tokenProperties("word").size-1).map( i => keys.map(k => tokenProperties(k)(i)).mkString("\t") ).mkString("\n")
  }
  def tag(implicit tagger:Any):Concordance = ???
  
  override def toString() = (f"${left}%80s") + " \u169b"  + hit + "\u169c " + right + " metadata: " + meta
 }




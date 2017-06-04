

case class Concordance(hitStart: Int, hitEnd: Int, tokenProp:  Map[String,Array[String]], meta: Map[String,String])
{

  val tokenProperties: Map[String,Array[String]] = tokenProp
  val metadata: Map[String,String] = meta
  val defaultProperty = "word"
  
  lazy val words = tokenProperties(defaultProperty)
  lazy val left = words.slice(0,hitStart).mkString(" ")
  lazy val right = words.slice(hitEnd, words.length).mkString(" ")
  lazy val hit = words.slice(hitStart, hitEnd).mkString(" ")
  
  def apply(field: String) = tokenProperties(field)
  
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
    val keys = tokenProperties.keys.toList
    //val triples = (0 to tokenProperties("word").size-1).map( i => keys.map(k => tokenProperties(k)(i)))
    def prefix(i:Int) = if (i==hitStart) ">" else ""
    (0 to tokenProperties("word").size-1).map( 
        i => prefix(i) + keys.map(k => tokenProperties(k)(i)).mkString("\t") 
     ).mkString("\n")
  }
  
  /**
   * Problem: tagger will retokenize, so we have to match back the best match
   */
  def tag(implicit tagger:Tagger):Concordance = 
  {
    val retokenized = this.retokenize(Tokenizer)
    val tagged = tagger.tag(retokenized("word").mkString(" "))
    val findMe = retokenized("word")(hitStart)
    val indexes = (0 to tagged("word").size -1).filter(tagged("word")(_) == findMe)
    val bestIndex = indexes.minBy(i => Math.abs(hitStart - i))
    this.copy(tokenProp=tagged,hitStart=bestIndex, hitEnd=bestIndex+1)
  }
  
  override def toString() = (f"${left}%80s") + " \u169b"  + hit + "\u169c " + right + " metadata: " + meta
 }




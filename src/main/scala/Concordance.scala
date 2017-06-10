import scala.xml._


case class Concordance(hitStart: Int, hitEnd: Int, tokenProperties:  Map[String,Array[String]], metadata: Map[String,String])
{
   val minEen:Int  = -1
  val defaultProperty = "word"
  
  lazy val words = tokenProperties(defaultProperty)
  lazy val left = words.slice(0,hitStart).mkString(" ")
  lazy val right = words.slice(hitEnd, words.length).mkString(" ")
  lazy val hit = words.slice(hitStart, hitEnd).mkString(" ")
  
  def apply(field: String):Array[String] = tokenProperties(field)
  def meta(field:String) = metadata(field)
  
  def retokenize(t:Tokenizer) = 
  {
    val tokens = tokenProperties("word").map(s => t.tokenize(s).head)
    val prepunct = tokens.map(t => t.leading)
    val postpunct = tokens.map(t => t.trailing)
    val words = tokens.map(t => t.token)
    val newProperties = tokenProperties -- List("word","prepunctuation","postpunctuation") ++ List("word" -> words, "prepunctuation" -> prepunct, "postpunctuation" -> postpunct)
    this.copy(tokenProperties=newProperties)
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
   * Problem: tagger will retokenize, so we have to match back the hit position. Rather ugly, and might not always work
   */
  
  def tag(implicit tagger:Tagger):Concordance = 
  {
    val retokenized = this // .retokenize(Tokenizer) // nee, dubbel tokenizeren is niet goed....
    val tagged = tagger.tag(retokenized("word").mkString(" "))
    if (hitStart >= retokenized("word").length)
      println(s"Miserie: $hitStart $retokenized ${retokenized("word").length}")

    val findMe = retokenized("word")(hitStart)
    val indexes = (0 to tagged("word").size -1).filter(tagged("word")(_) == findMe)
    if (indexes.isEmpty)
    {
        this.copy(hitStart=minEen,hitEnd=minEen,tokenProperties=tagged)
    }
    else
    {
    val bestIndex = indexes.minBy(i => Math.abs(hitStart - i))
    val r = this.copy(hitStart=bestIndex,hitEnd=bestIndex+1,tokenProperties=tagged)
    r
    }
  }
  
  override def toString() = (f"${left}%80s") + " \u169b"  + hit + "\u169c " + right + " metadata: " + metadata

  def toHTMLRow = <tr align="right"><td>{left}</td><td><b>{hit}</b></td><td>{right}</td></tr>
  def toHTMLItem = <li>{left} <b>{hit}</b> {right}</li>
 }

object Concordance
{
  def toHTMLTable(c: List[Concordance]):Node = <table>{c.map(_.toHTMLRow)}</table>
  def toHTMLList(c: List[Concordance]):Node = <ul>{c.map(_.toHTMLItem)}</ul>
  def toHTMLAsText(c: List[Concordance]):String = toHTMLTable(c).toString.replaceAll("\\s+", " ")

  def tagBatch(tagger: Tagger, c: Seq[Concordance]):Seq[Concordance] =
  {
    val startMarker = "开始"
    val sentences = c.map(startMarker + " " + _("word").mkString(" "))
    val tagged:Map[String,Array[String]] = tagger.tag(sentences.mkString(" "))
    val properties = tagged.keys.toList
    val startPositions = tagged("word").indices.filter(i => tagged("word")(i) == startMarker)
    val stukjes:Seq[Map[String, Array[String]]] = startPositions.indices.map(i =>
      {
        val s = startPositions(i) + 1
        val e = if (i == startPositions.size-1) tagged("word").length else startPositions(i+1)

        properties.map(p =>  p -> tagged("word").slice(s,e)    ).toMap
      }
    )
    def key(c:Concordance):String = c("word").mkString(" ")
    val cache = c.indices.map(i => key(c(i)) -> stukjes(i)).toMap

    object cachingTagger extends Tagger
    {
      override def urlFor(text: String): String = null

      override def tag(text: String): Map[String, Array[String]] = cache(text)
    }
    return c.map(c => c.tag(cachingTagger))
  }
}




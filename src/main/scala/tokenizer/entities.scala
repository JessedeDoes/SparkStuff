package tokenizer

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

  def substitute(s:String):String = substitute(s,defaultMapping)
  override def substitute(s:String, mapping:Map[String,String]):String =
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

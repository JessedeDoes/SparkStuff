trait TextQuery
{
	override def toString() = this match
			{
			case SingleTerm(s) => s
			case And(t1,t2) => "(" + t1.toString + "+AND+" + t2.toString + ")"
			case Or(t1,t2) => "(" + t1.toString + "+OR+" + t2.toString + ")"
			case Disjunction(l @ _*) => "(" + l.map(_.toString).mkString("+OR+")  + ")"
			case ListDisjunction(l) => "(" + l.map(_.toString).mkString("+OR+")  + ")"
			case ListConjunction(l) => "(" + l.map(_.toString).mkString("+AND+")  + ")"
			case Phrase(l @ _*) => "%22" + l.map(_.toString).mkString("+")  + "%22"
			}
}

case class SingleTerm(term:String) extends TextQuery
case class ExpandTerm(term:String) extends TextQuery
case class And(t1:TextQuery, t2:TextQuery) extends TextQuery
case class Or(t1:TextQuery, t2:TextQuery) extends TextQuery
case class Phrase(l: TextQuery*) extends TextQuery
case class Disjunction(l: TextQuery*) extends TextQuery
case class ListDisjunction(l: List[TextQuery]) extends TextQuery
case class ListConjunction(l: List[TextQuery]) extends TextQuery

// http://sk.taalbanknederlands.inl.nl/LexiconService/lexicon/get_wordforms?database=lexicon_service_db&lemma=bunzing


trait ContentQueryT
{
	def startDate:String
	val endDate:String
	val textQuery:TextQuery
	def toParameterValue():String ="date+within+%22" + startDate + "+" + endDate + "%22+AND+" + textQuery
}

trait SRUQueryT
{
	def server:String 
	def operation:String
	def collection:String 
	def startRecord:Int 
	def maximumRecords:Int
	def query:ContentQuery
	def mkURL(): String =
	server + "&operation=" + operation + "&x-collection=" + collection + "&startRecord=" + 
			startRecord + "&maximumRecords=" + maximumRecords + "&query=" + query.toParameterValue()
}

case class ContentQuery(startDate:String, 
    endDate:String, textQuery:TextQuery) extends ContentQueryT
    
case class SRUQuery(server:String, 
    operation:String, collection:String, 
    startRecord:Int, maximumRecords:Int, query:ContentQuery) extends SRUQueryT
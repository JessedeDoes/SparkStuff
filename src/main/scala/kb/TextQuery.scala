package kb

trait TextQuery
{
	def toQueryString():String = this match
			{
			case SingleTerm(s) => s
			case And(t1,t2) => "(" + t1.toQueryString + "+AND+" + t2.toQueryString + ")"
			case Or(t1,t2) => "(" + t1.toQueryString + "+OR+" + t2.toQueryString + ")"
			case Disjunction(l @ _*) => "(" + l.map(_.toQueryString).mkString("+OR+")  + ")"
			case ListDisjunction(l) => "(" + l.map(_.toQueryString).mkString("+OR+")  + ")"
			case ListConjunction(l) => "(" + l.map(_.toQueryString).mkString("+AND+")  + ")"
			case Phrase(l @ _*) => "%22" + l.map(_.toQueryString).mkString("+")  + "%22"
			}

}

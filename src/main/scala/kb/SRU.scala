package kb

object SRU
{
   implicit def wrapTextQuery(t:TextQuery):SRUQuery =
          SRUQuery(QueryKB.defaultServer, "searchRetrieve",
             QueryKB.defaultCollection, 0, QueryKB.maxDocuments,
             ContentQuery(QueryKB.defaultStartDate, QueryKB.defaultEndDate, t))

  def singleWordQuery(term:String):SRUQuery = wrapTextQuery(SingleTerm(term))
  implicit def StringToTerm(s:String):SingleTerm = SingleTerm(s)
  implicit def StringToQuery(s:String):SRUQuery = singleWordQuery(s)

  def termsIn(t: TextQuery):Set[String] =
  {
     t match
    {
      case SingleTerm(term) => Set(term)
      case And(t1,t2)  => termsIn(t1) ++ termsIn(t2)
      case Or(t1,t2) => termsIn(t1) ++ termsIn(t2)
      case Disjunction(l @ _*) => l.flatMap(termsIn).toSet
      case ListDisjunction(li) => li.flatMap(termsIn).toSet
      case ListConjunction(li) => li.flatMap(termsIn).toSet
      case Phrase(l @ _*) => l.flatMap(termsIn).toSet
    }
  }

   def cartesianProduct[A](l: List[List[A]]):List[List[A]] =
   {
      l match
      {
         case head :: tail if tail.size > 0 => head.flatMap(x =>  cartesianProduct(tail).map(y => x :: y))
         case head :: tail if tail.size == 0 => head.map ( a => List(a) )
         case _ => List.empty
      }
   }

  def expandQuery(f: String => List[String])(t: TextQuery):TextQuery =
  {
	  val expand:TextQuery=>TextQuery = expandQuery(f)
    t match
    {
      case SingleTerm(term) =>
        val l = f(term)
        val l1 = if (l.contains(term.toLowerCase)) l else term.toLowerCase :: l
        ListDisjunction(l1.map( x => SingleTerm(x)))
      case And(t1,t2) => And(expand(t1),expand(t2))
      case Or(t1,t2) => Or(expand(t1),expand(t2))
      case Disjunction(l @ _*) => Disjunction(l.map(expand):_*)
      case ListDisjunction(li) => ListDisjunction(li.map(expand))
      case ListConjunction(li) => ListConjunction(li.map(expand))
      case Phrase(l @ _*) => // pfft. dit moet toch simpeler kunnen? In ieder geval zou je beter de expansies filteren op voorkomen; of de lijst in queries splitsen
          val x = cartesianProduct(l.toList.map( { case SingleTerm(t) => f(t) }))
          val y = x.map(s =>  { val l = s.map(x => SingleTerm(x)); Phrase(l:_*) } )
          ListDisjunction(y)
    }
  }
}

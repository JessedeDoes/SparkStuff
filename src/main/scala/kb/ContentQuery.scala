package kb

case class ContentQuery(startDate:String,
    endDate:String, textQuery:TextQuery) extends ContentQueryT

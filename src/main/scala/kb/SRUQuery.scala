package kb

case class SRUQuery(server:String,
    operation:String, collection:String,
    startRecord:Int, maximumRecords:Int, query:ContentQuery) extends SRUQueryT

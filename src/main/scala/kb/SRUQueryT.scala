package kb

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

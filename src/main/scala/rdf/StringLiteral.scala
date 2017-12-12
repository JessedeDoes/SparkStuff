package rdf

case class StringLiteral(s:String) extends RDFNode with RDFLiteral[String]

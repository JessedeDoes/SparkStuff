package rdf

case class IntLiteral(i:Int) extends RDFNode with RDFLiteral[Int]

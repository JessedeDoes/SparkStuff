package rdf

case class DataProperty(subject:RDFResource, predicate:Predicate, obj:RDFLiteral[Any]) extends Statement

package rdf

case class ObjectProperty(subject:RDFResource, predicate:Predicate, obj:RDFResource) extends Statement

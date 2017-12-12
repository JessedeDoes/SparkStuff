package wsd

import concordance.Concordance

object wsdObject extends WSD
{
  def tag(in: Stream[Concordance], tagger: Concordance=>String):Stream[Concordance] = in.map(c => c.copy(metadata=c.metadata + ("senseId" -> tagger(c))))
}

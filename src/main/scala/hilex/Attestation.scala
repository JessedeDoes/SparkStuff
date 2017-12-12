package hilex

import concordance.Concordance
import tokenizer.TokenizerWithOffsets

case class Attestation(wordform: Wordform, quote: String, start_pos: Int, end_pos: Int, eg_id: String, document: DocumentMetadata) {
  lazy val senses = ???

  import tokenizer.TokenizerWithOffsets._


  def sillyCriterium(x: TokenWithOffsets): Int = {
    x match {
      case TokenWithOffsets(t, s, e) => Math.abs(s - start_pos + e - end_pos)
    }
  }

  def toConcordance = {
    val tokens = TokenizerWithOffsets.tokenize(quote)(really = false)
    val probableHit = tokens.toList.zipWithIndex.minBy(x => sillyCriterium(x._1))

    val tokenStream = tokens.toStream

    val c = new Concordance(probableHit._2, probableHit._2 + 1, List(("word", tokens.map(t => entities.substitute(t.token.token)))).toMap, document.properties)
    c
  }
}

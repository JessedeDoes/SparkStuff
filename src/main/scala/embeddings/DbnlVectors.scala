package embeddings

import word2vec.Vectors

object DbnlVectors extends VectorSpace {
  override lazy val vectors: Vectors = Vectors.readFromFile("/home/jesse/workspace/Diamant/Vectors/dbnl.vectors.bin")
}

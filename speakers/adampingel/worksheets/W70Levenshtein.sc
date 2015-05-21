object W70Levenshtein {

  // Levenshtein Distance

  import axle._
  import axle.nlp.Levenshtein
  import spire.implicits.IntAlgebra
  import spire.implicits.DoubleAlgebra
  import spire.algebra.Field
  import org.jblas.DoubleMatrix
  import axle.jblas._

  val space = Levenshtein[Vector, Char, DoubleMatrix, Int]()

  space.distance("the quick brown fox".toVector, "the quik brown fax".toVector)

  space.distance("the quick brown fox".toVector, "the quik brown fox".toVector)

  space.distance("the quick brown fox".toVector, "the quick brown fox".toVector)

}
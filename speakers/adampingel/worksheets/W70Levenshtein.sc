object W70Levenshtein {

  // Levenshtein Distance

  import axle._
  import axle.algebra._
  import axle.nlp.Levenshtein
  import spire.implicits.IntAlgebra
  import org.jblas.DoubleMatrix
  import axle.jblas._

  implicit val laJblasInt = linearAlgebraDoubleMatrix[Int]
                                                  //> laJblasInt  : axle.algebra.LinearAlgebra[org.jblas.DoubleMatrix,Int,Int,Int]
                                                  //|  = axle.jblas.package$$anon$10@445b295b

  implicit val space = Levenshtein[IndexedSeq, Char, DoubleMatrix, Int]()
                                                  //> space  : axle.nlp.Levenshtein[IndexedSeq,Char,org.jblas.DoubleMatrix,Int] = 
                                                  //| Levenshtein()

  space.distance("the quick brown fox", "the quik brown fax")
                                                  //> res0: Int = 2

  space.distance("the quick brown fox", "the quik brown fox")
                                                  //> res1: Int = 1

  space.distance("the quick brown fox", "the quick brown fox")
                                                  //> res2: Int = 0

  import spire.syntax.metricSpace.metricSpaceOps

  "bat" distance "baa"                            //> res3: Int = 1

}
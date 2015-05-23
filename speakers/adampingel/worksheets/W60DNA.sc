object W60DNA {

  // DNA Sequence Alignment
  // matrix + dynamic programming

  import axle._
  import axle.bio._
  import axle.jblas._

  import spire.implicits.DoubleAlgebra
  implicit val laJblasDouble = linearAlgebraDoubleMatrix[Double]
                                                  //> laJblasDouble  : axle.algebra.LinearAlgebra[org.jblas.DoubleMatrix,Int,Int,D
                                                  //| ouble] = axle.jblas.package$$anon$10@445b295b

  val (aNW, bNW) = NeedlemanWunsch.optimalAlignment("ATGCGGCC", "ATCGCCGG")
                                                  //> aNW  : String = ATGCGGCC--
                                                  //| bNW  : String = AT-C-GCCGG

  NeedlemanWunsch.alignmentScore(aNW, bNW)        //> res0: Double = 32.0

  val (aSW, bSW) = SmithWaterman.optimalAlignment("ACACACTA", "AGCACACA")
                                                  //> aSW  : String = A-CACACTA
                                                  //| bSW  : String = AGCACAC-A

  import SmithWaterman._

  // alignmentScore(aSW, bSW)

}
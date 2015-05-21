object W60DNA {
  
  // DNA Sequence Alignment
  // matrix + dynamic programming

  import axle._
  import axle.bio._
  import axle.jblas._
 
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
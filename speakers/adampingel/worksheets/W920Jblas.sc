object W920Jblas {

  import axle._
  import axle.jblas._

  import spire.implicits.DoubleAlgebra
  implicit val laJblasDouble = axle.jblas.linearAlgebraDoubleMatrix[Double]
                                                  //> laJblasDouble  : axle.algebra.LinearAlgebra[org.jblas.DoubleMatrix,Int,Int,D
                                                  //| ouble] = axle.jblas.package$$anon$10@5c671d7f
  import laJblasDouble._

  // Note: dimensions would be needed for "zero" matrix

  show(ones(2, 3))                                //> 1.000000 1.000000 1.000000
                                                  //| 1.000000 1.000000 1.000000

  show(matrix(2, 2, List(1.1, 2.2, 3.3, 4.4).toArray))
                                                  //> 1.100000 3.300000
                                                  //| 2.200000 4.400000

  show(rand(3, 3))                                //> 0.289827 0.907975 0.010273
                                                  //| 0.790138 0.857253 0.231316
                                                  //| 0.798048 0.381477 0.277714

  show(matrix(4, 5, (r, c) => r / (c + 1d)))      //> 0.000000 0.000000 0.000000 0.000000 0.000000
                                                  //| 1.000000 0.500000 0.333333 0.250000 0.200000
                                                  //| 2.000000 1.000000 0.666667 0.500000 0.400000
                                                  //| 3.000000 1.500000 1.000000 0.750000 0.600000

  show(matrix(5, 5, 0d,
    (r: Int) => r + 0.5,
    (c: Int) => c + 0.6,
    (r: Int, c: Int, diag: Double, left: Double, right: Double) => diag))
                                                  //> 0.000000 1.600000 2.600000 3.600000 4.600000
                                                  //| 1.500000 0.000000 1.600000 2.600000 3.600000
                                                  //| 2.500000 1.500000 0.000000 1.600000 2.600000
                                                  //| 3.500000 2.500000 1.500000 0.000000 1.600000
                                                  //| 4.500000 3.500000 2.500000 1.500000 0.000000

  val m = matrix(4, 5, (1 to 20).map(_.toDouble).toArray)
                                                  //> m  : org.jblas.DoubleMatrix = [1.000000, 5.000000, 9.000000, 13.000000, 17.0
                                                  //| 00000; 2.000000, 6.000000, 10.000000, 14.000000, 18.000000; 3.000000, 7.0000
                                                  //| 00, 11.000000, 15.000000, 19.000000; 4.000000, 8.000000, 12.000000, 16.00000
                                                  //| 0, 20.000000]

  show(m)                                         //> 1.000000 5.000000 9.000000 13.000000 17.000000
                                                  //| 2.000000 6.000000 10.000000 14.000000 18.000000
                                                  //| 3.000000 7.000000 11.000000 15.000000 19.000000
                                                  //| 4.000000 8.000000 12.000000 16.000000 20.000000

  import axle.syntax.linearalgebra.matrixOps
  show(m.slice(1 to 3, 2 to 4))                   //> 10.000000 14.000000 18.000000
                                                  //| 11.000000 15.000000 19.000000
                                                  //| 12.000000 16.000000 20.000000


  implicit val endo = axle.jblas.endoFunctorDoubleMatrix[Double]
                                                  //> endo  : axle.algebra.Endofunctor[org.jblas.DoubleMatrix,Double] = axle.jblas
                                                  //| .package$$anon$7@66f57048
  import axle.syntax.endofunctor.endofunctorOps


  show(m.map(_ + 1d))                             //> 2.000000 6.000000 10.000000 14.000000 18.000000
                                                  //| 3.000000 7.000000 11.000000 15.000000 19.000000
                                                  //| 4.000000 8.000000 12.000000 16.000000 20.000000
                                                  //| 5.000000 9.000000 13.000000 17.000000 21.000000

}
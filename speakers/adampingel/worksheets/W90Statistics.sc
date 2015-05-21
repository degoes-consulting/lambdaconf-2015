object W90Statistics {

  import axle._
  import axle.stats._
  import spire.math._
  import spire.algebra._

  val dist = uniformDistribution(List(2d, 4d, 4d, 4d, 5d, 5d, 7d, 9d), "some doubles")
                                                  //> dist  : axle.stats.Distribution0[Double,spire.math.Rational] = ConditionalPr
                                                  //| obabilityTable0(Map(5.0 -> 1/4, 9.0 -> 1/8, 2.0 -> 1/8, 7.0 -> 1/8, 4.0 -> 3
                                                  //| /8),some doubles)
 
  import spire.implicits.DoubleAlgebra

  standardDeviation(dist)                         //> res0: Double = 2.0


  // Distribution Monad
  //  d6 + d6

  import axle.game.Dice.die
 
  val distribution = for {
    a <- die(6)
    b <- die(6)
  } yield a + b                                   //> distribution  : axle.stats.Distribution0[Int,spire.math.Rational] = Conditio
                                                  //| nalProbabilityTable0(Map(5 -> 1/9, 10 -> 1/12, 6 -> 5/36, 9 -> 1/9, 2 -> 1/3
                                                  //| 6, 12 -> 1/36, 7 -> 1/6, 3 -> 1/18, 11 -> 1/18, 8 -> 5/36, 4 -> 1/12),unname
                                                  //| d)
  
  distribution.values.sorted.foreach { v =>
    val p = distribution.probabilityOf(v)
    println(s"$v  $p")
  }                                               //> 2  1/36
                                                  //| 3  1/18
                                                  //| 4  1/12
                                                  //| 5  1/9
                                                  //| 6  5/36
                                                  //| 7  1/6
                                                  //| 8  5/36
                                                  //| 9  1/9
                                                  //| 10  1/12
                                                  //| 11  1/18
                                                  //| 12  1/36


}
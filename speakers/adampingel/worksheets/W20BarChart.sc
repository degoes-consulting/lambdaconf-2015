object W20BarChart {

  // guessed from https://twitter.com/lambda_conf/status/594264379425574913

  val ageGroupTally = Map[Int, Double](
    20 -> 2.6,
    25 -> 8.9,
    30 -> 10,
    35 -> 7.5,
    40 -> 4.5,
    45 -> 3.5,
    50 -> 1.2,
    55 -> 0.8,
    60 -> 0.3,
    65 -> 0.5,
    70 -> 0.0,
    75 -> 0.0,
    80 -> 0.3
  )                                               //> ageGroupTally  : scala.collection.immutable.Map[Int,Double] = Map(25 -> 8.9,
                                                  //|  20 -> 2.6, 60 -> 0.3, 70 -> 0.0, 65 -> 0.5, 45 -> 3.5, 80 -> 0.3, 35 -> 7.5
                                                  //| , 50 -> 1.2, 40 -> 4.5, 55 -> 0.8, 75 -> 0.0, 30 -> 10.0)
  import axle._
  import axle.visualize._
  import spire.implicits.IntAlgebra
  import spire.implicits.DoubleAlgebra
  import java.awt.Color.blue

  val chart = BarChart[Int, Double, Map[Int, Double]](
    ageGroupTally,
    xAxis = 0d,
    drawKey = false,
    title = Some("LambdaConf 2015 Age Distribution"),
    colors = List(blue)
  )                                               //> chart  : axle.visualize.BarChart[Int,Double,Map[Int,Double]] = BarChart(Map(
                                                  //| 25 -> 8.9, 20 -> 2.6, 60 -> 0.3, 70 -> 0.0, 65 -> 0.5, 45 -> 3.5, 80 -> 0.3,
                                                  //|  35 -> 7.5, 50 -> 1.2, 40 -> 4.5, 55 -> 0.8, 75 -> 0.0, 30 -> 10.0),false,70
                                                  //| 0,600,50,0.8,20,50,80,Some(LambdaConf 2015 Age Distribution),Courier New,12,
                                                  //| Palatino,20,0.0,None,None,UnittedQuantity(36.0,UnitOfMeasurement(degree,°,S
                                                  //| ome(http://en.wikipedia.org/wiki/Degree_(angle)))),List(java.awt.Color[r=0,g
                                                  //| =0,b=255]))\

 // draw(chart)
}
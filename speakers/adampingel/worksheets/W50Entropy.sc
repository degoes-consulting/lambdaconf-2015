object W50Entropy {

  import axle.quanta.Information
  import scala.collection.immutable.TreeMap
  import axle.quanta.UnittedQuantity
  import spire.math.Rational
 
  type D = TreeMap[Rational, UnittedQuantity[Information, Double]]
 
  import spire.implicits.DoubleAlgebra
  import axle.jung.JungDirectedGraph

  implicit val id = Information.converterGraph[Double, JungDirectedGraph]
                                                  //> id  : axle.quanta.UnitConverterGraph[axle.quanta.Information,Double,axle.jun
                                                  //| g.JungDirectedGraph] with axle.quanta.InformationConverter[Double] = axle.qu
                                                  //| anta.Information$$anon$1@4c309d4d
 
  import axle.stats.H
  import axle.stats.coin
 
  val hm: D =
    new TreeMap[Rational, UnittedQuantity[Information, Double]]() ++
      (0 to 100).map(i => (Rational(i / 100d), H(coin(Rational(i, 100))))).toMap
                                                  //> hm  : W50Entropy.D = Map(0 -> UnittedQuantity(0.0,UnitOfMeasurement(bit,b,No
                                                  //| ne)), 5764607523034235/576460752303423488 -> UnittedQuantity(0.0807931358959
                                                  //| 1118,UnitOfMeasurement(bit,b,None)), 5764607523034235/288230376151711744 -> 
                                                  //| UnittedQuantity(0.14144054254182067,UnitOfMeasurement(bit,b,None)), 10808639
                                                  //| 10568919/36028797018963968 -> UnittedQuantity(0.19439185783157623,UnitOfMeas
                                                  //| urement(bit,b,None)), 5764607523034235/144115188075855872 -> UnittedQuantity
                                                  //| (0.24229218908241482,UnitOfMeasurement(bit,b,None)), 3602879701896397/720575
                                                  //| 94037927936 -> UnittedQuantity(0.28639695711595625,UnitOfMeasurement(bit,b,N
                                                  //| one)), 1080863910568919/18014398509481984 -> UnittedQuantity(0.3274449191544
                                                  //| 7627,UnitOfMeasurement(bit,b,None)), 1261007895663739/18014398509481984 -> U
                                                  //| nittedQuantity(0.36592365090022316,UnitOfMeasurement(bit,b,None)), 576460752
                                                  //| 3034235/72057594037927936 -> UnittedQuantity(0.4021791902022729,UnitOfMeasur
                                                  //| ement(bit,b,None)), 3242
                                                  //| Output exceeds cutoff limit.
 
  import axle.visualize._
  implicit val bitDouble = id.bit                 //> bitDouble  : axle.quanta.UnitOfMeasurement[axle.quanta.Information] = UnitOf
                                                  //| Measurement(bit,b,None)
 
  val plot = new Plot[Rational, UnittedQuantity[Information, Double], D](
    List(("h", hm)),
    connect = true,
    drawKey = false,
    xAxis = Some(0d *: bitDouble),
    xAxisLabel = Some("p(x='HEAD)"),
    yAxis = Some(Rational(0)),
    yAxisLabel = Some("H"),
    title = Some("Coin Flip Entropy vs Bias"))    //> plot  : axle.visualize.Plot[spire.math.Rational,axle.quanta.UnittedQuantity[
                                                  //| axle.quanta.Information,Double],W50Entropy.D] = Plot(List((h,Map(0 -> Unitte
                                                  //| dQuantity(0.0,UnitOfMeasurement(bit,b,None)), 5764607523034235/5764607523034
                                                  //| 23488 -> UnittedQuantity(0.08079313589591118,UnitOfMeasurement(bit,b,None)),
                                                  //|  5764607523034235/288230376151711744 -> UnittedQuantity(0.14144054254182067,
                                                  //| UnitOfMeasurement(bit,b,None)), 1080863910568919/36028797018963968 -> Unitte
                                                  //| dQuantity(0.19439185783157623,UnitOfMeasurement(bit,b,None)), 57646075230342
                                                  //| 35/144115188075855872 -> UnittedQuantity(0.24229218908241482,UnitOfMeasureme
                                                  //| nt(bit,b,None)), 3602879701896397/72057594037927936 -> UnittedQuantity(0.286
                                                  //| 39695711595625,UnitOfMeasurement(bit,b,None)), 1080863910568919/180143985094
                                                  //| 81984 -> UnittedQuantity(0.32744491915447627,UnitOfMeasurement(bit,b,None)),
                                                  //|  1261007895663739/18014398509481984 -> UnittedQuantity(0.36592365090022316,U
                                                  //| nitOfMeasurement(bit,b,N
                                                  //| Output exceeds cutoff limit.
 
  implicit val ut = axle.quanta.unittedTics[Information, Double, JungDirectedGraph]
                                                  //> ut  : axle.algebra.Tics[axle.quanta.UnittedQuantity[axle.quanta.Information
                                                  //| ,Double]] = axle.quanta.package$$anon$5@387a8303\

  // ut.tics(0d *: bitDouble, 4d *: bitDouble)

  // draw(plot)
}
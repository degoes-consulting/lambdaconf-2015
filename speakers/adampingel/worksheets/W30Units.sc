object W30Units {

  import axle._
  import axle.quanta._
  import axle.jung._

  import spire.implicits.IntAlgebra
  import spire.implicits.DoubleAlgebra
  import axle.algebra.modules.doubleRationalModule

  implicit val massConverter = Mass.converterGraph[Double, JungDirectedGraph]
  import massConverter._

  implicit val distanceConverter = Distance.converterGraph[Double, JungDirectedGraph]
  import distanceConverter._

  1 *: gram

  import spire.implicits.additiveSemigroupOps
  import spire.implicits.additiveGroupOps
  import spire.implicits.moduleOps

  val x = (10d *: gram) + (1d *: kilogram)

  show(x)

  val y = (1.01 *: kilogram) in gram

  show(y)

  import spire.algebra._

  implicitly[AdditiveMonoid[UnittedQuantity[Distance, Double]]]

  def f1[T](q: UnittedQuantity[Distance, T]): UnittedQuantity[Distance, T] =
    ???

  def f2[T: Field: AdditiveMonoid](q: UnittedQuantity[Distance, T]): UnittedQuantity[Distance, T] =
    implicitly[AdditiveMonoid[UnittedQuantity[Distance, T]]].plus(q, q)

}
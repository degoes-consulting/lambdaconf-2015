object W30Units {

  import axle._
  import axle.quanta._
  import axle.jung._
  import spire.implicits.DoubleAlgebra

  implicit val mass = Mass.converterGraph[Double, JungDirectedGraph]
                                                  //> mass  : axle.quanta.UnitConverterGraph[axle.quanta.Mass,Double,axle.jung.Jun
                                                  //| gDirectedGraph] with axle.quanta.MassConverter[Double] = axle.quanta.Mass$$a
                                                  //| non$1@1b6e1eff

  import mass.{ gram, kilogram }

  2d *: gram                                      //> res0: axle.quanta.UnittedQuantity[axle.quanta.Mass,Double] = UnittedQuantity
                                                  //| (2.0,UnitOfMeasurement(gram,g,None))

  show(2d *: gram)                                //> 2.000000 g

  import spire.implicits.additiveSemigroupOps

  val x = (10d *: gram) + (1d *: kilogram)        //> x  : axle.quanta.UnittedQuantity[axle.quanta.Mass,Double] = UnittedQuantity(
                                                  //| 1.01,UnitOfMeasurement(kilogram,Kg,None))

  show(x)                                         //> 1.010000 Kg

  import spire.implicits.additiveGroupOps

  val y = (10d *: gram) - (1d *: kilogram)        //> y  : axle.quanta.UnittedQuantity[axle.quanta.Mass,Double] = UnittedQuantity(
                                                  //| -0.99,UnitOfMeasurement(kilogram,Kg,None))

  show(y)                                         //> -0.990000 Kg

  import spire.implicits.moduleOps

  val z = (1.01 *: kilogram) in gram              //> z  : axle.quanta.UnittedQuantity[axle.quanta.Mass,Double] = UnittedQuantity(
                                                  //| 1010.0,UnitOfMeasurement(gram,g,None))

  show(z)                                         //> 1010.000000 g

  def s[Q, T: Show](q: UnittedQuantity[Q, T]): Unit =
    show(q)                                       //> s: [Q, T](q: axle.quanta.UnittedQuantity[Q,T])(implicit evidence$2: axle.Sho
                                                  //| w[T])Unit

  case class Cat(name: String, age: Double)

  implicit def showCat: Show[Cat] = new Show[Cat] {
    def text(c: Cat): String = s"cat ${c.name}, age ${c.age} years"
  }                                               //> showCat: => axle.Show[W30Units.Cat]

  val cody = Cat("Cody", 8.5)                     //> cody  : W30Units.Cat = Cat(Cody,8.5)
  val sunny = Cat("Sunny", 5d)                    //> sunny  : W30Units.Cat = Cat(Sunny,5.0)

  show(cody)                                      //> cat Cody, age 8.5 years

  s(cody *: gram)                                 //> cat Cody, age 8.5 years g

  // No Group defined on Cats :(
  //
  // (cody *: gram) - (sunny *: gram)
  //

  import spire.algebra.Field

  def double[Q, T: Field](
    q: UnittedQuantity[Q, T])(
      implicit converter: UnitConverter[Q, T]): UnittedQuantity[Q, T] =
    2 *: q                                        //> double: [Q, T](q: axle.quanta.UnittedQuantity[Q,T])(implicit evidence$3: sp
                                                  //| ire.algebra.Field[T], implicit converter: axle.quanta.UnitConverter[Q,T])ax
                                                  //| le.quanta.UnittedQuantity[Q,T]

  show(double(1d *: gram))                        //> 2.000000 g

  // import axle.algebra.modules.doubleRationalModule

}
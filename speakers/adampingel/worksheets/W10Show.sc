object W10Show {
  
  import axle._

  case class Cat(name: String, age: Double)

  implicit def showCat: Show[Cat] = new Show[Cat] {

    def text(c: Cat): String = s"cat ${c.name}, age ${c.age} years"
  }                                               //> showCat: => axle.Show[W10Show.Cat]

  val cody = Cat("Cody", 8.5)                     //> cody  : W10Show.Cat = Cat(Cody,8.5)

  show(cody)                                      //> cat Cody, age 8.5 years

  string(cody)                                    //> res0: String = cat Cody, age 8.5 years

}
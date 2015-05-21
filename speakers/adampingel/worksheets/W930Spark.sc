object W930Spark {

  // followed changes to Spire's Σ and Monoid.sum https://github.com/non/spire/commits?author=adampingel

  // From spire.optional.unicode:
  //
  //   def Σ[A](as: Iterable[A])(implicit ev: AdditiveMonoid[A]): A =
  //     as.aggregate(ev.zero)(ev.plus, ev.plus)
  //
  // How can I get an RDD to do this?
  //
  // axle.spark._ defines Aggregatable[RDD]
  //

  import scala.math.random
  import spire.implicits.IntAlgebra
  import org.apache.spark.SparkConf
  import org.apache.spark.SparkContext
  
  import axle.algebra.Aggregatable
  import axle.algebra.Finite
  import axle.algebra.Functor
  import axle.algebra.Σ
  import axle.spark._

  val randomPointInCircle: () => Int = () => {
    val x = random * 2 - 1
    val y = random * 2 - 1
    if (x * x + y * y < 1) 1 else 0
  }                                               //> randomPointInCircle  : () => Int = <function0>

  def estimatePi[F[_]: Functor: Aggregatable, N](trials: F[Int])(implicit finite: Finite[F, Long]): Double = {
    val n = finite.size(trials)
    val numInCircle = implicitly[Functor[F]].map(trials)(i => randomPointInCircle())
    4d * Σ(numInCircle) / n
  }                                               //> estimatePi: [F[_], N](trials: F[Int])(implicit evidence$3: axle.algebra.Fun
                                                  //| ctor[F], implicit evidence$4: axle.algebra.Aggregatable[F], implicit finite
                                                  //| : axle.algebra.Finite[F,Long])Double

  val slices = 2                                  //> slices  : Int = 2
  val n = 100000 * slices                         //> n  : Int = 200000
  val conf = new SparkConf().setAppName("Pi on Axle on Spark").setMaster("local[2]")
                                                  //> conf  : org.apache.spark.SparkConf = org.apache.spark.SparkConf@71623278
  val spark = new SparkContext(conf)              //> Using Spark's default log4j profile: org/apache/spark/log4j-defaults.proper
                                                  //| ties
                                                  //| 15/05/20 01:47:16 INFO SparkContext: Running Spark version 1.3.0
                                                  //| 15/05/20 01:47:16 WARN NativeCodeLoader: Unable to load native-hadoop libra
                                                  //| ry for your platform... using builtin-java classes where applicable
                                                  //| 15/05/20 01:47:16 INFO SecurityManager: Changing view acls to: pingel
                                                  //| 15/05/20 01:47:16 INFO SecurityManager: Changing modify acls to: pingel
                                                  //| 15/05/20 01:47:16 INFO SecurityManager: SecurityManager: authentication dis
                                                  //| abled; ui acls disabled; users with view permissions: Set(pingel); users wi
                                                  //| th modify permissions: Set(pingel)
                                                  //| 15/05/20 01:47:17 INFO Slf4jLogger: Slf4jLogger started
                                                  //| 15/05/20 01:47:17 INFO Remoting: Starting remoting
                                                  //| 15/05/20 01:47:17 INFO Remoting: Remoting started; listening on addresses :
                                                  //| [akka.tcp://sparkDriver@10.0.0.4:56515]
                                                  //| 15/05/20 01:47:17 INFO Utils: Successfully started service 'sparkDriver' on
                                                  //|  p
                                                  //| Output exceeds cutoff limit.

  // val trials = spark.parallelize(1 to n, slices)
  // val trials = (1 to n).toList
  // val trials = 1 to n
  val trials = (1 to n).par                       //> trials  : scala.collection.parallel.immutable.ParRange = ParRange(1, 2, 3, 
                                                  //| 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 2
                                                  //| 4, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 
                                                  //| 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61,
                                                  //|  62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80
                                                  //| , 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 9
                                                  //| 9, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 11
                                                  //| 4, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128, 12
                                                  //| 9, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 14
                                                  //| 4, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 15
                                                  //| 9, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 17
                                                  //| 4, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 18
                                                  //| 9, 190, 191, 192, 193, 
                                                  //| Output exceeds cutoff limit.

  val π = estimatePi(trials)                      //> π  : Double = 3.14764

  println(s"π ≅ $π")                              //> π ≅ 3.14764

  spark.stop()                                    //> 15/05/20 01:47:20 INFO ContextHandler: stopped o.s.j.s.ServletContextHandle
                                                  //| r{/metrics/json,null}
                                                  //| 15/05/20 01:47:20 INFO ContextHandler: stopped o.s.j.s.ServletContextHandle
                                                  //| r{/stages/stage/kill,null}
                                                  //| 15/05/20 01:47:20 INFO ContextHandler: stopped o.s.j.s.ServletContextHandle
                                                  //| r{/,null}
                                                  //| 15/05/20 01:47:20 INFO ContextHandler: stopped o.s.j.s.ServletContextHandle
                                                  //| r{/static,null}
                                                  //| 15/05/20 01:47:20 INFO ContextHandler: stopped o.s.j.s.ServletContextHandle
                                                  //| r{/executors/threadDump/json,null}
                                                  //| 15/05/20 01:47:20 INFO ContextHandler: stopped o.s.j.s.ServletContextHandle
                                                  //| r{/executors/threadDump,null}
                                                  //| 15/05/20 01:47:20 INFO ContextHandler: stopped o.s.j.s.ServletContextHandle
                                                  //| r{/executors/json,null}
                                                  //| 15/05/20 01:47:20 INFO ContextHandler: stopped o.s.j.s.ServletContextHandle
                                                  //| r{/executors,null}
                                                  //| 15/05/20 01:47:20 INFO ContextHandler: stopped o.s.j.s.ServletContextHandle
                                                  //| r{/environment/json,null}
                                                  //| 15/05/20 01:
                                                  //| Output exceeds cutoff limit.
}
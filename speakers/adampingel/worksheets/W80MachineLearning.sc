object W80MachineLearning {

  // Naive Bayes

  case class Tennis(outlook: String, temperature: String, humidity: String, wind: String, play: Boolean)
 
  val data = Tennis("Sunny", "Hot", "High", "Weak", false) ::
      Tennis("Sunny", "Hot", "High", "Strong", false) ::
      Tennis("Overcast", "Hot", "High", "Weak", true) ::
      Tennis("Rain", "Mild", "High", "Weak", true) ::
      Tennis("Rain", "Cool", "Normal", "Weak", true) ::
      Tennis("Rain", "Cool", "Normal", "Strong", false) ::
      Tennis("Overcast", "Cool", "Normal", "Strong", true) ::
      Tennis("Sunny", "Mild", "High", "Weak", false) ::
      Tennis("Sunny", "Cool", "Normal", "Weak", true) ::
      Tennis("Rain", "Mild", "Normal", "Weak", true) ::
      Tennis("Sunny", "Mild", "Normal", "Strong", true) ::
      Tennis("Overcast", "Mild", "High", "Strong", true) ::
      Tennis("Overcast", "Hot", "Normal", "Weak", true) ::
      Tennis("Rain", "Mild", "High", "Strong", false) :: Nil
                                                  //> data  : List[W80MachineLearning.Tennis] = List(Tennis(Sunny,Hot,High,Weak,fa
                                                  //| lse), Tennis(Sunny,Hot,High,Strong,false), Tennis(Overcast,Hot,High,Weak,tru
                                                  //| e), Tennis(Rain,Mild,High,Weak,true), Tennis(Rain,Cool,Normal,Weak,true), Te
                                                  //| nnis(Rain,Cool,Normal,Strong,false), Tennis(Overcast,Cool,Normal,Strong,true
                                                  //| ), Tennis(Sunny,Mild,High,Weak,false), Tennis(Sunny,Cool,Normal,Weak,true), 
                                                  //| Tennis(Rain,Mild,Normal,Weak,true), Tennis(Sunny,Mild,Normal,Strong,true), T
                                                  //| ennis(Overcast,Mild,High,Strong,true), Tennis(Overcast,Hot,Normal,Weak,true)
                                                  //| , Tennis(Rain,Mild,High,Strong,false))

  // Build a classifier to predict the Boolean feature 'play' given all the other features of the observations
  
  import axle._
  import axle.stats._
  import axle.ml.NaiveBayesClassifier
  import spire.algebra._
  import spire.math._
 
  val classifier = NaiveBayesClassifier(
    data,
    List(
      UnknownDistribution0[String, Rational](Vector("Sunny", "Overcast", "Rain"), "Outlook"),
      UnknownDistribution0[String, Rational](Vector("Hot", "Mild", "Cool"), "Temperature"),
      UnknownDistribution0[String, Rational](Vector("High", "Normal", "Low"), "Humidity"),
      UnknownDistribution0[String, Rational](Vector("Weak", "Strong"), "Wind")
    ),
    UnknownDistribution0[Boolean, Rational](Vector(true, false), "Play"),
    (t: Tennis) => t.outlook :: t.temperature :: t.humidity :: t.wind :: Nil,
    (t: Tennis) => t.play)                        //> classifier  : axle.ml.NaiveBayesClassifier[W80MachineLearning.Tennis,String
                                                  //| ,Boolean,List] = <function1>
 
  data map { datum => datum.toString + "\t" + classifier(datum) } mkString("\n")
                                                  //> res0: String = Tennis(Sunny,Hot,High,Weak,false)	false
                                                  //| Tennis(Sunny,Hot,High,Strong,false)	false
                                                  //| Tennis(Overcast,Hot,High,Weak,true)	true
                                                  //| Tennis(Rain,Mild,High,Weak,true)	true
                                                  //| Tennis(Rain,Cool,Normal,Weak,true)	true
                                                  //| Tennis(Rain,Cool,Normal,Strong,false)	true
                                                  //| Tennis(Overcast,Cool,Normal,Strong,true)	true
                                                  //| Tennis(Sunny,Mild,High,Weak,false)	false
                                                  //| Tennis(Sunny,Cool,Normal,Weak,true)	true
                                                  //| Tennis(Rain,Mild,Normal,Weak,true)	true
                                                  //| Tennis(Sunny,Mild,Normal,Strong,true)	true
                                                  //| Tennis(Overcast,Mild,High,Strong,true)	true
                                                  //| Tennis(Overcast,Hot,Normal,Weak,true)	true
                                                  //| Tennis(Rain,Mild,High,Strong,false)	false

  import axle.ml.ClassifierPerformance
  import spire.implicits.DoubleAlgebra
 
  val perf = ClassifierPerformance[Double, Tennis, List](data, classifier, _.play)
                                                  //> perf  : axle.ml.ClassifierPerformance[Double,W80MachineLearning.Tennis,List
                                                  //| ] = ClassifierPerformance(List(Tennis(Sunny,Hot,High,Weak,false), Tennis(Su
                                                  //| nny,Hot,High,Strong,false), Tennis(Overcast,Hot,High,Weak,true), Tennis(Rai
                                                  //| n,Mild,High,Weak,true), Tennis(Rain,Cool,Normal,Weak,true), Tennis(Rain,Coo
                                                  //| l,Normal,Strong,false), Tennis(Overcast,Cool,Normal,Strong,true), Tennis(Su
                                                  //| nny,Mild,High,Weak,false), Tennis(Sunny,Cool,Normal,Weak,true), Tennis(Rain
                                                  //| ,Mild,Normal,Weak,true), Tennis(Sunny,Mild,Normal,Strong,true), Tennis(Over
                                                  //| cast,Mild,High,Strong,true), Tennis(Overcast,Hot,Normal,Weak,true), Tennis(
                                                  //| Rain,Mild,High,Strong,false)),<function1>,<function1>)

  perf.precision                                  //> res1: Double = 0.9
  perf.recall                                     //> res2: Double = 0.6923076923076923
  perf.f1Score                                    //> res3: Double = 0.7826086956521738

  val perfR = ClassifierPerformance[Rational, Tennis, List](data, classifier, _.play)
                                                  //> perfR  : axle.ml.ClassifierPerformance[spire.math.Rational,W80MachineLearni
                                                  //| ng.Tennis,List] = ClassifierPerformance(List(Tennis(Sunny,Hot,High,Weak,fal
                                                  //| se), Tennis(Sunny,Hot,High,Strong,false), Tennis(Overcast,Hot,High,Weak,tru
                                                  //| e), Tennis(Rain,Mild,High,Weak,true), Tennis(Rain,Cool,Normal,Weak,true), T
                                                  //| ennis(Rain,Cool,Normal,Strong,false), Tennis(Overcast,Cool,Normal,Strong,tr
                                                  //| ue), Tennis(Sunny,Mild,High,Weak,false), Tennis(Sunny,Cool,Normal,Weak,true
                                                  //| ), Tennis(Rain,Mild,Normal,Weak,true), Tennis(Sunny,Mild,Normal,Strong,true
                                                  //| ), Tennis(Overcast,Mild,High,Strong,true), Tennis(Overcast,Hot,Normal,Weak,
                                                  //| true), Tennis(Rain,Mild,High,Strong,false)),<function1>,<function1>)

  perfR.precision                                 //> res4: spire.math.Rational = 9/10
  perfR.recall                                    //> res5: spire.math.Rational = 9/13
  perfR.f1Score                                   //> res6: spire.math.Rational = 18/23

  // TODO Linear Regression, KMeans, Confusion Matrix, MAP@k

}
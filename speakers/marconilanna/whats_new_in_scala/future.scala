import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object future extends App {

	def f(tag: String, ms: Int) = Future {
		println(s"$tag started")
		Thread.sleep(ms)
		println(s"$tag ended")
		tag
	}

	for {
		a <- f("a", 500)
		b <- f("b", 100)
		c <- f("c", 300)
	} println(a + b + c)

	Thread.sleep(1000)

	val fa = f("A", 500)
	val fb = f("B", 300)
	val fc = f("C", 250)

	for {
		a <- fa
		b <- fb
		c <- fc
	} println(a + b + c)

	Thread.sleep(1000)
}

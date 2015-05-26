import System.{currentTimeMillis => now}
import scala.util.Random

object seq extends App {
	val n = 50 * 1000 * 1000
	val max = 2 * n

	def random = Random nextInt max

	val col = Vector.fill(n)(random)
	val target = random

	val start = now

	col.count(math.sqrt(_) == target)
	val middle = now

	col.count(math.sqrt(_) == target)
	val end = now

	println("seq results:")
	println("1st run: " + (middle-start) + "ms")
	println("2nd run: " + (end-middle) + "ms")
}

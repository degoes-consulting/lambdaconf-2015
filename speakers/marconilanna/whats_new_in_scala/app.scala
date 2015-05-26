import System.{currentTimeMillis => now}

object app extends App {
	val n = 4 * 1000 * 1000

	val start = now

	var sum = 0L
	for (i <- 1 to n) sum += i
	val middle = now

	sum = 0L
	for (i <- 1 to n) sum += i
	val end = now

	println("app results:")
	println("1st run: " + (middle-start) + "ms")
	println("2nd run: " + (end-middle) + "ms")
}

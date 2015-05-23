import System.{currentTimeMillis => now}

object app2 extends App {
	val n = 4 * 1000 * 1000

	val start = now

	var i, sum = 0L
	while (i < n) {
		i += 1
		sum += i
	}
	val middle = now

	i = 0L
	sum = 0L
	while (i < n) {
		i += 1
		sum += i
	}
	val end = now

	println("app2 results:")
	println("1st run: " + (middle-start) + "ms")
	println("2nd run: " + (end-middle) + "ms")
}

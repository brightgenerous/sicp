println("1.1.7")
println("--")

def sqrt(x: Double) = sqrtItr(1.0, x)

def sqrtItr(guess: Double, x: Double): Double =
  guess match {
    case y if goodEnough(y, x) => y
    case y => sqrtItr(improve(y, x), x)
  }

def goodEnough(guess: Double, x: Double) =
  Math.abs(guess * guess - x) < 0.001

def improve(guess: Double, x: Double) =
  (x / guess + guess) / 2

Seq(2, 3, 4, 5, 16, 100, 999)
  .foreach(x => println(s"sqrt($x) => ${sqrt(x)}"))

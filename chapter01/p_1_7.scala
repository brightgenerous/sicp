println("問題1.7")
println("--")

def sqrt(x: Double) = {
  val startGuess = improve(1.0, x)
  val startDist = if (x >= 1) x * x else 1 / (x * x)
  sqrtItr(startGuess, x, startDist)
}

def sqrtItr(guess: Double, x: Double, dist: Double): Double =
  getDist(guess, x) match {
    case d if d >= dist => guess
    case d => sqrtItr(improve(guess, x), x, d)
  }

def getDist(guess: Double, x: Double) = {
  // for debug
  // println(s"x => $x, guess => $guess, Math.abs(guess * guess - x) => ${Math.abs(guess * guess - x)}")
  Math.abs(guess * guess - x)
}

def improve(guess: Double, x: Double) =
  (x / guess + guess) / 2

Seq(0.1, 1, 2, 3, 4, 5, 16, 100, 999, 1000000000000000.0)
  .foreach(x => println(s"sqrt($x) => ${sqrt(x)}"))

println("問題1.8")
println("--")

/* --

  // y != 0

    y^3 = x

  .div y^2
    y = x / y^2

  .div 3
    y / 3 = (x / y^2) / 3

  .add (2 * y) / 3
    y / 3 + (2 * y) / 3 = (x / y^2) / 3 + (2 * y) / 3

  .->
    (y + 2 * y) / 3 = ((x / y^2) + (2 * y)) / 3

  .->
    y = ((x / y^2) + (2 * y)) / 3

-- */

def cubic(x: Double) =
  x match {
    case a if a >= 0 => cubicAbs(a)
    case a => cubicAbs(a * -1) * -1
  }

def cubicAbs(x: Double) = { require(x >= 0)
  val startGuess = improve(1.0, x)
  val startDist = x match {
    case a if a >= 1 => a * a * a
    case a => 1 / (a * a * a)
  }
  cubicItr(startGuess, x, startDist)
}

def cubicItr(guess: Double, x: Double, dist: Double): Double =
  getDist(guess, x) match {
    case d if d >= dist => guess
    case d => cubicItr(improve(guess, x), x, d)
  }

def getDist(guess: Double, x: Double) = {
  // for debug
  // println(s"x => $x, guess => $guess, Math.abs(guess * guess * guess - x) => ${Math.abs(guess * guess * guess - x)}")
  Math.abs(guess * guess * guess - x)
}

def improve(guess: Double, x: Double) =
  (x / (guess * guess) + 2 * guess) / 3

Seq(0.0001, 0.001, 0.1, 1, 2, 3, 4, 5, 16, 100, 999, 1000000000000000.0).flatMap(x => Seq(x, x * -1))
  .foreach(x => println(s"cubic($x) => ${cubic(x)}"))

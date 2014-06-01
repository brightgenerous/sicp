val tolerance = 0.00001

def fixedPoint(f: Function1[Double, Double], firstGuess: Double) = {
  val closeEnough = (v1: Double, v2: Double) =>
    Math.abs(v1 - v2) < tolerance
  // try は予約後な
  def tri(guess: Double, count: Int): Double =
    Option(guess).map(f)
      .map(next => { println(s"\t$count:$next"); next })
      .get match {
        case next if closeEnough(guess, next) => next
        case next => tri(next, count + 1)
      }
  tri(firstGuess, 1)
}

def sqrt(x: Double) = fixedPoint((y: Double) => (x / y + y) * 0.5, 1.0)
println(s"sqrt(2.0) => ${sqrt(2.0)}")
println("----------")

def goldenRatio = fixedPoint((y: Double) => 1 + 1 / y, 1.0)
println(s"goldenRatio => ${goldenRatio}")
println("----------")

def xx = fixedPoint((y: Double) => Math.log(1000) / Math.log(y), 1.1)
println(s"xx => ${xx}")
println("----------")

def xxAD = fixedPoint((y: Double) => (Math.log(1000) / Math.log(y) + y) * 0.5, 1.1)
println(s"xx => ${xxAD}")

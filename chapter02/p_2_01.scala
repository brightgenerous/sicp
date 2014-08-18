case class Rational(number:Int, denom:Int) {

  def add(r:Rational) =
    Rational(number * r.denom + r.number * denom, denom * r.denom)

  def sub(r:Rational) =
    Rational(number * r.denom - r.number * denom, denom * r.denom)

  def mul(r:Rational) =
    Rational(number * r.number, denom * r.denom)

  def div(r:Rational) =
    Rational(number * r.denom, number * r.denom)

  def eql(r:Rational) =
    number * r.denom == denom * r.number

  def gcd = {
    val g = getGcd(number, denom)
    new Rational(number / g, denom / g)
  }

  private def getGcd(x:Int, y:Int): Int =
    y match {
      case 0 => x
      case _ => getGcd(y, x % y)
    }
}


println("---------------------")
println(s"Rational(6,9).number => ${Rational(6,9).number}")
println(s"Rational(6,9).denom => ${Rational(6,9).denom}")
println(s"Rational(2,3) => ${Rational(2,3)}")
println(s"Rational(6,9) => ${Rational(6,9)}")
println(s"Rational(6,9).eql(Rational(2,3)) => ${Rational(6,9).eql(Rational(2,3))}")
println(s"Rational(6,9).equals(Rational(2,3)) => ${Rational(6,9).equals(Rational(2,3))}")
println(s"Rational(6,9).gcd.equals(Rational(2,3).gcd) => ${Rational(6,9).gcd.equals(Rational(2,3).gcd)}")
println("---------------------")

Rational(6,9).gcd match {
  case Rational(2,x) => println(s"Rational(2,x) x => ${x}")
  case _ => println("other")
}


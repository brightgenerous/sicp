println("問題1.16")
println("--")

def fastExpt(x: Int, y: Int) = {
  val absY = Math.abs(y)
  def fastExptItr(count: Int, res: Int): Int =
    count match {
      case c if c == absY => res
      case c if c * 2 <= absY =>
        fastExptItr(c * 2, res * res)
      case c =>
        fastExptItr(c + 1, res * x)
    }
  (x, y) match {
    case (0, _) => 0
    case (1, _) => 1
    case (_, 0) => 1
    case (_, 1) => x
    case (x, y) if y > 0 => fastExptItr(1, x)
    case (x, y) => 1.0 / fastExptItr(1, x)
  }
}

Seq(1, 2, 3, 4, 5)
  .flatMap(r => Stream.continually(r).take(r + 1).zipWithIndex)
  .flatMap(tup => Seq(tup, (tup._1, tup._2 * -1)))
  .foreach(tup => println(s"$tup => ${fastExpt(tup._1, tup._2)}"))

println("問題1.18")
println("--")

def mul(x: Int, y: Int) = {
  def mulItr(count: Int, to: Int, step: Int, current: Int, total: Int): Int = {
    count match {
      case c if c == to => total + current
      case c if c + c <= to + c / 2 =>
        mulItr(c + c, to, step, current + current, total)
      case c if c < to =>
        mulItr(1, to - c, step, step, total + current)
      case c =>
        mulItr(1, c - to, step * -1, step * -1, total + current)
    }
  }
  lazy val absX = Math.abs(x)
  lazy val absY = Math.abs(y)
  lazy val absMax = Math.max(absX, absY)
  lazy val absMin = Math.min(absX, absY)
  (x, y) match {
    case (x, y) if x ==0 || y == 0 => 0
    case (x, y) if (x > 0 && y > 0) || (x < 0 && y < 0) =>
      mulItr(1, absMin, absMax, absMax, 0)
    case (x, y) =>
      mulItr(1, absMin, absMax, absMax, 0) * -1
  }
}

Seq(1, 2, 3, 4, 5, 20)
  .flatMap(r => Stream.continually(r).take(r + 1).zipWithIndex)
  .flatMap(tup => Seq(tup, (tup._1, tup._2 * -1)))
  .foreach(tup => println(s"$tup => ${mul(tup._1, tup._2)}"))

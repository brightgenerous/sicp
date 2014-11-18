object Main {

  def main(args: Array[String]) = {
    val n = Console.readLine.toInt
    val lines = Array.ofDim[Seq[Int]](n + 1)
    for (i <- 1 until n) {
      val Seq(a, b) = Console.readLine.split(' ').toSeq.map(_.toInt)
      if (lines(a) == null) {
        lines(a) = Seq(b)
      } else {
        lines(a) = b +: lines(a)
      }
      if (lines(b) == null) {
        lines(b) = Seq(a)
      } else {
        lines(b) = a +: lines(b)
      }
    }
    val q = Console.readLine.toInt
    for (i <- 0 until q) {
      val Seq(a, b) = Console.readLine.split(' ').toSeq.map(_.toInt)
      println(itr(lines, Seq(a), b, Nil, 1))
    }
  }

  def itr(lines: Array[Seq[Int]],
      froms: Seq[Int], to: Int, ignore: Seq[Int], count: Int): Int = {
    val tos = froms.flatMap { from =>
      lines(from) match {
        case null => Nil
        case t => t.filter(e => !ignore.contains(e))
      }
    }
    if (tos.contains(to)) {
      count + 1
    } else {
      itr(lines, tos, to, froms, count + 1)
    }
  }
}


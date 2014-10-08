//# coding: utf-8

// https://codeiq.jp/ace/thisweek_masuipeo/q1110

// scala 2.11

object Main {

  def main(args: Array[String]) = {
    println(patterns(8).size)
  }

  def patterns(n: Int) = {
    val revs = reverses(n)
    (0 until n).permutations
      .filter { pattern => ok(revs, pattern) }
  }

  def ok(revs: Seq[Seq[Int]], pattern: Seq[Int]) = {
    val sands = pattern.map { p => (p, p) }
    val fall = sands.map { sand => (sand._1, sand._2 - 1) }
    val rs = revs.head
    val rev = fall.map {
        case (p, s) if rs.contains(p) =>
          (p, p)
        case ps => ps
      }
    true
  }

  def reverses(n: Int) =
    Seq(
      Seq(0),
      Seq(1, 2),
      Seq(2, 3, 4),
      Seq(3, 4, 5, 6),
      Seq(4, 5, 6, 7, 0),
      Seq(5, 6, 7, 0, 1, 2),
      Seq(6, 7, 0, 1, 2, 3, 4),
      Seq(7, 0, 1, 2, 3, 4, 5, 6)
    )
}


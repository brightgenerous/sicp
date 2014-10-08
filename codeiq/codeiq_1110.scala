//# coding: utf-8

// https://codeiq.jp/ace/thisweek_masuipeo/q1110

// scala 2.11

// answer
//   6055

object Main {

  def main(args: Array[String]) = {
    val t = System.currentTimeMillis

    println(patterns(8).size)

    // 1350 ms
    println((System.currentTimeMillis - t) + " ms")
  }

  def patterns(n: Int) =
    (1 to n).permutations
      .filter { pattern => println(""); ok(n, pattern) }

  // (capacity: Int, current: Int)
  type SandWatch = (Int, Int)

  // (index: Int, SandWatch)
  type Step = (Int, Seq[SandWatch])

  def ok(n: Int, pattern: Seq[Int]) = {
    def itr(count: Int, step: Seq[SandWatch],
            steps: Seq[Step]): Boolean = {

      // finish?
      if (step.forall { case (_, s) => s == 1 }) {
        return true;
      }

      // fall
      val fall = step.map { case (t, s) => (t, Math.max(s - 1, 0)) }

      // reverse
      val indexes = reverseIndexes(n, count % n, pattern(count % n))
      val rev = fall.zipWithIndex.map {
          case ((t, s), i) if indexes.contains(i) =>
            (t, t - s)
          case ((t, s), i) => (t, s)
        }
      val last = (count % n, rev)

      // contains?
      if (steps.contains(last)) {
        return false;
      }

      //println(last)

      // go to next
      itr(count + 1, rev, last +: steps)
    }

    // start!!
    itr(0, pattern.map { i => (i, i) }, Seq())
  }

  def reverseIndexes(n: Int, start: Int, sand: Int) =
    (start until (start + sand)).map(_ % n)
}


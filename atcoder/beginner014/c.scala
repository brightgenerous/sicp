object Main {

  def main(args: Array[String]) = {
    val n = Console.readLine.toInt
    val memo =  Array.ofDim[Int](1000001, 2)
    for (i <- 0 until n) {
      val Seq(a, b) = Console.readLine.split(' ').toSeq.map(_.toInt)
      memo(a)(0) += 1
      memo(b)(1) += 1
    }
    var tmp, res = 0
    for (se <- memo) {
      tmp += se(0)
      if (res < tmp) {
        res = tmp
      }
      tmp -= se(1)
    }
    println(res)
  }
}

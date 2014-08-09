println("問題1.12")
println("--")

def pascalRec(r: Int, c: Int): Int =
  (r, c) match {
    case (0, _) => 1
    case (_, 0) => 1
    case (r, c) if r == c => 1
    case _ => pascalRec(r - 1, c - 1) + pascalRec(r - 1, c)
  }

def pascal(r: Int, c: Int): Int = {
  def incrementRow(values: List[Int]) = {
    val tmp = 0 :: values
    tmp.zip(tmp.reverse).map(tup => tup._1 + tup._2)
  }
  def getRow(r: Int) = {
    def getRowItr(c: Int, res: List[Int]): List[Int] =
      c match {
        case 0 => res
        case _ => getRowItr(c - 1, incrementRow(res))
      }
    getRowItr(r, List(1))
  }
  (r, c) match {
    case (0, _) => 1
    case (_, 0) => 1
    case (r, c) if r == c => 1
    case _ => getRow(r)(c)
  }
}

def printExecutionTime(desc: String)(proc: => Unit) = {
  println(s"-- $desc -- ")
  val start = System.currentTimeMillis
  proc
  println((System.currentTimeMillis - start) + "msec")
}

printExecutionTime("recursive") {
  Seq(0, 1, 2, 3, 4, 5)
    .flatMap(r => Stream.continually(r).take(r + 1).zipWithIndex)
    .foreach(rc => println(s"$rc => ${pascalRec(rc._1, rc._2)}"))
}

printExecutionTime("iterative") {
  Seq(0, 1, 2, 3, 4, 5)
    .flatMap(r => Stream.continually(r).take(r + 1).zipWithIndex)
    .foreach(rc => println(s"$rc => ${pascal(rc._1, rc._2)}"))
}

//# coding: utf-8

// https://codeiq.jp/ace/ozy4dm/q1105

// scala 2.11

object Main {
  def data(size: Int) = {
    val lt = (1 to size)
      .map { i => (1 to size).map { _ <= i } }
    val lb = lt.map(_.reverse)
    lt.zip(lt.reverse).map { case (a, b) => a ++ b } ++
      lb.zip(lb.reverse).map { case (a, b) => a ++ b }
  }

  def main(args: Array[String]) {
    (6 to 7).map {
      data(_)
        .map { _.map { case true => "*"; case _ => " " }.mkString }
        .mkString("\n")
    }.foreach(println)
  }
}


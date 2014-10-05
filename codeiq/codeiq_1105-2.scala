//# coding: utf-8

// https://codeiq.jp/ace/ozy4dm/q1105

// scala 2.11

object Main {

  val chars = (n: Int) => (c: Char) => c.toString * n

  def data(c: Char)(size: Int) = {
    val lt = (1 to size).map { i => Seq(chars(0), chars(i), chars(size - i), chars(0)) }
    val lb = lt.map(chars(0) +: _.reverse :+ chars(0))
    val matrix = lt.zip(lt.reverse).map { case(a, b) => a ++ b } ++
                 lb.zip(lb.reverse).map { case(a, b) => a ++ b }
    matrix.map {
      _.zipWithIndex.map {
        case (cs, i) if i % 2 == 0 => cs(' ')
        case (cs, _) => cs(c)
      }.mkString
    }.mkString("\n")
  }

  def main(args: Array[String]) {
    val kz = data('*')_
    (5 to 7).map(kz).foreach(println)
  }
}


//# coding: utf-8

// https://codeiq.jp/ace/nabetani_takenori/q1097

/*
Answer
31,41,59,265,358,444,555,666,777,888,979,999
*/


object Main {

  val patternAB =  """.*(?:AB|BA)=([0-9]+)cm.*""".r
  val patternBC =  """.*(?:BC|CB)=([0-9]+)cm.*""".r
  val patternCA =  """.*(?:CA|AC)=([0-9]+)cm.*""".r
  val patternA  =  """.*角A=([0-9]+)度.*""".r
  val patternB  =  """.*角B=([0-9]+)度.*""".r
  val patternC  =  """.*角C=([0-9]+)度.*""".r

  def main(args:Array[String]) = {
    val lines = scala.io.Source.fromFile("codeiq_1097.data.utf8.txt")
      .getLines
    val answer = lines.filterNot(line => line.isEmpty)
      .map { line =>
        val strs = line.trim.split("\\s+")
        val str = Option(strs(1))
        val ab = str.collect( s => s match {
          case patternAB(v) => v.toInt
        })
        val bc = str.collect( s => s match {
          case patternBC(v) => v.toInt
        })
        val ca = str.collect( s => s match {
          case patternCA(v) => v.toInt
        })
        val a = str.collect( s => s match {
          case patternA(v) => v.toInt
        })
        val b = str.collect( s => s match {
          case patternB(v) => v.toInt
        })
        val c = str.collect( s => s match {
          case patternC(v) => v.toInt
        })
        (strs(0), (ab, bc, ca, a, b, c), strs(2))
      }.map { data =>
        val tt = getTriangleType(
            data._2._1, data._2._2, data._2._3,
            data._2._4, data._2._5, data._2._6
          ) match {
            case 1 => "あ"
            case 2 => "い"
            case 3 => "う"
          }
        (data._1, data._2, data._3, tt)
      }.filterNot(data => data._3 == data._4)
      .map(data => data._1)
      .mkString(",")

     println(answer)
  }

  def getTriangleType(ab:Option[Int], bc:Option[Int], ca:Option[Int],
              a:Option[Int], b:Option[Int], c:Option[Int]) =
    (ab, bc, ca, a, b, c) match {
      // 正三角形
        // ３つの角度が等しい
      case (_, _, _, Some(60), Some(60), _) => 1
      case (_, _, _, Some(60), _, Some(60)) => 1
      case (_, _, _, _, Some(60), Some(60)) => 1

        // ２辺が等しく、60度の角度がある
      case (Some(ab), Some(bc), _, Some(60), _, _)
        if ab == bc => 1
      case (Some(ab), Some(bc), _, _, Some(60), _)
        if ab == bc => 1
      case (Some(ab), Some(bc), _, _, _, Some(60))
        if ab == bc => 1
      case (Some(ab), _, Some(ca), Some(60), _, _)
        if ab == ca => 1
      case (Some(ab), _, Some(ca), _, Some(60), _)
        if ab == ca => 1
      case (Some(ab), _, Some(ca), _, _, Some(60))
        if ab == ca => 1
      case (_, Some(bc), Some(ca), Some(60), _, _)
        if bc == ca => 1
      case (_, Some(bc), Some(ca), _, Some(60), _)
        if bc == ca => 1
      case (_, Some(bc), Some(ca), _, _, Some(60))
        if bc == ca => 1

        // ３辺が等しい
      case (Some(ab), Some(bc), Some(ca), _, _, _)
        if ab == bc && bc == ca => 1

      // 二等辺三角形
        // ２つの角度が等しい
      case (_, _, _, Some(a), Some(b), _)
        if a == b || a * 2 + b == 180 || a + b * 2 == 180 => 2
      case (_, _, _, Some(a), _, Some(c))
        if a == c || a * 2 + c == 180 || a + c * 2 == 180 => 2
      case (_, _, _, _, Some(b), Some(c))
        if b == c || b * 2 + c == 180 || b + c * 2 == 180 => 2

        // ２辺が等しい
      case (Some(ab), _, Some(ca), _, _, _)
        if ab == ca => 2
      case (Some(ab), Some(bc), _, _, _, _)
        if ab == bc => 2
      case (_, Some(bc), Some(ca), _, _, _)
        if bc == ca => 2

      // その他
      case _ => 3
  }
}


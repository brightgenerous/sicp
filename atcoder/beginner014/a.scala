object Main {

  def main(args: Array[String]) = {
    val a = Console.readLine.toInt
    val b = Console.readLine.toInt
    val res = if (a <= b) {
      b - a
    } else if (a % b == 0) {
      0
    } else {
      (a / b  + 1) * b - a
    }
    println(res)
  }
}

object Main {

  def main(args: Array[String]) = {
    val Seq(a, b) = Console.readLine.split(' ').toSeq.map(_.toInt)
    val costs = Console.readLine.split(' ').toSeq.map(_.toInt)
    def itr(costs: Seq[Int], b: Int, res: Int):Int = {
      b match {
        case 0 => res
        case b if b % 2 == 0 =>
          itr(costs.tail, b >>> 1, res)
        case _ =>
          itr(costs.tail, b >>> 1, res + costs.head)
      }
    }
    println(itr(costs, b, 0))
  }
}


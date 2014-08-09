println("問題1.11")
println("--")

/*
def fRec(n: Int): Int = {
  n match {
    case c if c < 3 => c
    case c => f(c - 1) + f(c - 2) * 2 + f(c - 3) * 3
  }
}
*/

def f(x: Int) = {
  def fItr(n1: Int, n2: Int, n3:Int, count: Int): Int = {
    count match {
      case 0 => n1
      case c => {
        val next = n1 + n2 * 2 + n3 * 3
        fItr(next, n1, n2, c - 1)
      }
    }
  }
  x match {
    case c if c < 3 => c
    case c => fItr(2, 1, 0, x - 2)
  }
}

Seq(0, 1, 2, 3, 4, 5, 16, 100, 999, 100000)
  .foreach(x => println(s"f($x) => ${f(x)}"))

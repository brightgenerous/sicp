println("問題1.37")
println("--")

def contFrac(n: Int => Double,  d: Int => Double, k: Int) = {
  def contFracItr(res: Double, k: Int): Double =
    k match {
      case 0 => res
      case _ => contFracItr(n(k) / (res + d(k)), k - 1)
    }
  contFracItr(0, k)
}

(1 to 20).toSeq.zipAll(Nil, 0, (i: Int) => 1.0).foreach({ case(i, f) =>
  println(s"contFrac(f, f, ${i}) => ${contFrac(f, f, i)}")
})

println("--")

def contFracR(n: Int => Double,  d: Int => Double, k: Int): Double =
  k match {
    case 0 => 0
    case _ => n(k) / (d(k) + contFracR(n, d, k - 1))
  }

(1 to 20).toSeq.zipAll(Nil, 0, (i: Int) => 1.0).foreach({ case(i, f) =>
  println(s"contFracR(f, f, ${i}) => ${contFracR(f, f, i)}")
})

println("----------")

println("問題1.38")
println("--")

def euler(n: Int) =
  Stream.range(n, 0, -1).flatMap(i => Seq(1.0, i * 2.0, 1.0))
    .reduce((z, n) => 1.0 / (n + z))

(1 until 10).foreach(i => println(s"euler($i) => ${euler(i)}"))

println("----------")

println("問題1.39")
println("--")

def tanCf(x: Double, n: Int) = {
  val sx = x * x
  Stream.range(n, 0, -1).map(_ * 2.0 - 1)
    .reduce((z, n) => sx / (n - z)) / x
}

Seq(1.0/2, 1.0/4, 1.0/6).foreach(r => {
  val rag = Math.PI * r
  (1 until 10).foreach(i => println(s"tanCf(PI * ${"%.2f".format(r)}, $i) => ${tanCf(rag, i)}"))
  println("--")
})

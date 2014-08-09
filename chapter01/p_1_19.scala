println("問題1.19")
println("--")

/*

a1 <- b * q + a * q + a * p
b1 <- b * p + a * q

a2 <- b1 * q + a1 * q + a1 * p
   <- (bp + aq) q + (bq + aq + ap) q + (bq + aq + ap) p
   <- bpq  + aq^2  + bq^2 + aq^2 + apq  + bpq + apq + ap^2
   <- bpq  + 2aq^2 + bq^2        + apq  + bpq + apq + ap^2
   <- 2bpq + 2aq^2 + bq^2        + apq        + apq + ap^2
   <- 2bpq + 2aq^2 + bq^2        + 2apq             + ap^2
   <- a (2q^2 + 2pq + p^2) + b (2pq + q^2)
   <- b (2pq + q^2) + a (2pq + q^2) + a (p^2 + q^2)

b2 <- b1 * p + a1 * q
   <- (bp + aq) p + (bq + aq + ap) q
   <- bp^2 + apq + bq^2 + aq^2 + apq
   <- b (p^2 + q^2) + a (2pq + q^2)

*/

def fib(n: Int) =
  fibItr(1, 0, 0, 1, n)

def fibItr(a: Int, b: Int, p: Int, q: Int, n: Int): Int =
  n match {
    case 0 => b
    case n if n % 2 == 0 =>
      fibItr(a, b, p * p + q * q, 2 * p * q + q * q, n / 2)
    case _ =>
      fibItr(b * q + a * q + a * p, b * p + a * q, p, q, n - 1)
  }

0.to(10)
  .foreach(n => println(s"fib($n) => ${fib(n)}"))

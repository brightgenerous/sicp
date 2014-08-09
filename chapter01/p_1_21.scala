println("問題1.21")
println("--")

def smallestDivisor(n: Int) = {
  def findDivisor(n: Int, testDivisor: Int): Int =
    testDivisor match {
      case t if t * t > n => n
      case t if isDivides(t, n) => t
      case t => findDivisor(n, t + 1)
    }
  def isDivides(a: Int, b: Int) =
    b % a == 0
  findDivisor(n, 2)
}

def isPrime(n: Int) =
  smallestDivisor(n) == n

2.to(20)
  .filter(isPrime)
  .foreach(n => println(s"$n is Prime"))

println("--")

Seq(199, 1999, 19999)
  .foreach(n => println(s"smallestDivisor($n) => ${smallestDivisor(n)}"))

println("-----")

println("問題1.22")
println("--")

def primeWithTime(n: Int) = {
  val start = System.nanoTime
  n match {
    case n if isPrime(n) =>
      (true, n, System.nanoTime - start)
    case _ => (false, n, 0)
  }
}

Seq(1000, 10000, 100000, 1000000).foreach(x => {
  Stream.from(x, -1).map(primeWithTime).filter(_._1).take(3).foreach(println)
  Stream.from(x, 1).map(primeWithTime).filter(_._1).take(3).foreach(println)
})

println("-----")

println("問題1.23")
println("--")

def isPrime2(n: Int) = {
  def smallestDivisor(n: Int) = {
    def findDivisor(n: Int, testDivisor: Int): Int =
      testDivisor match {
        case t if t * t > n => n
        case t if isDivides(t, n) => t
        case t => findDivisor(n, next(t))
      }
    def isDivides(a: Int, b: Int) =
      b % a == 0
    def next(n: Int) =
      n match {
        case 2 => 3
        case n => n + 2
      }
    findDivisor(n, 2)
  }
  smallestDivisor(n) == n
}

def prime2WithTime(n: Int) = {
  val start = System.nanoTime
  n match {
    case n if isPrime2(n) =>
      (true, n, System.nanoTime - start)
    case _ => (false, n, 0)
  }
}

Seq(1000, 10000, 100000, 1000000).foreach(x => {
  Stream.from(x, -1).map(prime2WithTime).filter(_._1).take(3).foreach(println)
  Stream.from(x, 1).map(prime2WithTime).filter(_._1).take(3).foreach(println)
})

println("-----")

println("問題1.24")
println("--")

def fastPrime(n: Int, times: Int): Boolean = {
  lazy val random = scala.util.Random
  def expmod(base: Int, exp: Int, m: Int): Int = {
    exp match {
      case 0 => 1
      case e if e % 2 == 0 =>
        square(expmod(base, e / 2, m)) % m
      case e =>
        base * expmod(base, e - 1, m) % m
    }
  }
  def square(x: Int) = x * x
  def fermatTest(n: Int) = {
    def tryIt(a: Int) =
      expmod(a, n, n) == a
    tryIt(1 + random.nextInt(n - 1))
  }
  times match {
    case 0 => true
    case t if fermatTest(n) =>
      fastPrime(n, times - 1)
    case t => false
  }
}

def fastPrimeWithTime(n: Int) = {
  val start = System.nanoTime
  n match {
    case n if fastPrime(n, 100) =>
      (true, n, System.nanoTime - start)
    case _ => (false, n, 0)
  }
}

Seq(1000, 10000, 100000, 1000000).foreach(x => {
  Stream.from(x, -1).map(prime2WithTime).filter(_._1).take(3).foreach(println)
  Stream.from(x, 1).map(prime2WithTime).filter(_._1).take(3).foreach(println)
})

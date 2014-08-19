type Church = (Int => Int) => (Int => Int)
//type Church = Function1[Int, Int] => Function1[Int, Int]

class ChurchValue(church: Church) {
  def value = church(i => i + 1)(0)
}
implicit def conv(church: Church) = new ChurchValue(church)

def zero(f: Int => Int)(x: Int) = x
//val zero = (f: Int => Int) => (x: Int) => x

def addOne(n: Church)(f: Int => Int)(x: Int) = f(n(f)(x))
/*
def addOne(n: Church): Church = {
  val _n = n
  (f: Int => Int) => {
    val _f = f
    (x: Int) => {
      _f ( _n ( _f ) ( x ) )
    }
  }
}
*/

def one(f: Int => Int)(x: Int) = f(x)
def two(f: Int => Int)(x: Int) = f(f(x))
def plus(a: Church, b: Church)(f: Int => Int)(x: Int) = a(f)(b(f)(x))

def disp(desc: String)(church: => Church) =
  println(s"$desc -> ${church.value}")

disp("zero") { zero }
disp("addOne(zero)") { addOne(zero) }
disp("addOne(addOne(zero))") { addOne(addOne(zero)) }
disp("one") { one }
disp("two") { two }
disp("plus(one, two)") { plus(one, two) }

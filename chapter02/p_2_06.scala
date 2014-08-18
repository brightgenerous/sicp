type Church = (Int => Int) => (Int => Int)
//type Church = Function1[Int, Int] => Function1[Int, Int]

def zero(f: Int => Int)(x: Int) = x
//val zero = (f: Int => Int) => (x: Int) => x

def addOne(n: Church)(f: Int => Int)(x: Int) = f(n(f)(x))
/*
def addOne(n: Church): Church = {
  (f: Int => Int) => {
    (x: Int) => {
      f ( n ( f ) ( x ) )
    }
  }
}
*/

def one(f: Int => Int)(x: Int) = f(x)
def two(f: Int => Int)(x: Int) = f(f(x))
def plus(a: Church, b: Church)(f: Int => Int)(x: Int) = a(f)(b(f)(x))

def disp(desc: String)(church: => Church) =
  println(s"$desc -> ${church(i => i + 1)(0)}")

disp("zero") { zero }
disp("addOne(zero)") { addOne(zero) }
disp("addOne(addOne(zero))") { addOne(addOne(zero)) }
disp("one") { one }
disp("two") { two }
disp("plus(one, two)") { plus(one, two) }

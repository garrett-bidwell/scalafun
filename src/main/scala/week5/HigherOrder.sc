def scaleList(xs: List[Double], factor: Double): List[Double] = xs match {
  case Nil => xs
  case y :: ys => y * factor :: scaleList(ys, factor)
}

def scaleListMap(xs: List[Double], factor: Double) =
  xs map(x => x * factor)

def squareList(xs: List[Int]): List[Int] = xs match {
  case Nil => Nil
  case y :: ys => y * y :: squareList(ys)
}

def squareListMap(xs: List[Int]): List[Int] =
  xs map(x => x * x)

def posElems(xs: List[Int]): List[Int] = xs match {
  case Nil => xs
  case y :: ys => if (y > 0) y :: posElems(ys) else posElems(ys)
}

def posElemsFilter(xs: List[Int]): List[Int] =
  xs filter(x => x > 0)

val nums = List(2, -4, 7, 1)
val fruits = List("pineapple", "banana", "orange", "apple")

nums filter(x => x > 0)
nums filterNot(x => x > 0)
// partion is like applying filter and filterNot and returning the results as a pair
nums partition(x => x > 0)
// takeWhile returns the longest prefix of the list that satisfies the predicate
nums takeWhile(x => x > 0)
// dropWhile returns the remainder of the list without the prefix taken by takeWhile
nums dropWhile(x => x > 0)
// span is a combination of takeWhile and dropWhile
nums span(x => x > 0)

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 =>
    val (first, rest) = xs span(y => y == x)
    first :: pack(rest)
}

val data = List("a","a","a","b","c","c","a")
pack(data)

def encode[T](xs: List[T]): List[(T, Int)] =
  pack(xs) map(ys => (ys.head, ys.length))

encode(data)

def sum(xs: List[Int]): Int = xs match {
  case Nil => 0
  case y :: ys => y + sum(ys)
}

def product(xs: List[Int]): Int = xs match {
  case Nil => 1
  case y :: ys => y * product(ys)
}

def sumReduce(xs: List[Int]): Int = (0 :: xs) reduceLeft((x, y) => x + y)
def productReduce(xs: List[Int]): Int = (1 :: xs) reduceLeft((x, y) => x * y)
// instead of ((x, y) => x + y) we can write (_ + _)
def sumReduce2(xs: List[Int]): Int = (0 :: xs) reduceLeft(_ + _)
def productReduce2(xs: List[Int]): Int = (1 :: xs) reduceLeft(_ * _)
// foldLeft is like reduceLeft, but it takes an accumulator as an additional argument
def sumFoldLeft(xs: List[Int]): Int = (xs foldLeft 0)(_ + _)
def productFoldLeft(xs: List[Int]): Int = (xs foldLeft 1)(_ *U _)

// for operators that are associative and commutative, foldLeft and foldRight are equivalent
def concat[T](xs: List[T], ys: List[T]): List[T] =
  (xs foldRight ys)(_ :: _)  // doesn't work with foldLeft, because :: is not defined for type T

def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())( ??? )

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)( ??? )
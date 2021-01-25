def isort(xs: List[Int]): List[Int] = xs match {
  case List() => List()
  case y :: ys => insert(y, isort(ys))
}

def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case List() => List(x)
  case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
}

isort(List(7, 3, 9, 2))
isort(List(3, 2))
List(2, 3) :+ 4

def last[T](xs: List[T]): T = xs match {
  case List() => throw new Error("last called on empty list")
  case List(x) => x
  case y :: ys => last(ys)
}

// returns a list consisting of all elements of xs except the last one
def init[T](xs: List[T]): List[T] = xs match {
  case List() => throw new Error("init called on empty list")
  case List(x) => List()
  case y :: ys => y :: init(ys)
}

def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
  case List() => ys
  case z :: zs => z :: concat(zs, ys)
}

def reverse[T](xs: List[T]): List[T] = xs match {
  case List() => xs
  case y :: ys => reverse(ys) ++ List(y)
}

def removeAt[T](n: Int, xs: List[T]) = (xs take n) ::: (xs drop n+1)

last(List(7, 3, 9, 2))
init(List(7, 3, 9, 2))
concat(List(7, 3, 9, 2), List(4, 1))
reverse(List(7, 3, 9, 2))
removeAt(2, List(7, 3, 9, 2))
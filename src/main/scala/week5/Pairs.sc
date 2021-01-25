
import math.Ordering

def msort(xs: List[Int]): List[Int] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    /*
    def merge(xs: List[Int], ys: List[Int]) = {
      xs match {
        case Nil => ys
        case x :: xs1 =>
          ys match {
            case Nil => xs
            case y :: ys1 =>
              if (x < y) x :: merge(xs1, ys)
              else y :: merge(xs, ys1)
          }
      }
    }
    */
    def merge(xs: List[Int], ys: List[Int]): List[Int] =
      (xs, ys) match {
        case (xs, Nil) => xs
        case (Nil, ys) => ys
        case (x :: xs1, y :: ys1) =>
          if (x < y) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }

    val (first, second) = xs splitAt n
    merge(msort(first), msort(second))
  }
}

val nums = List(2, -4, 7, 1)
msort(nums)

def msort_parameterized[T](xs: List[T])(lt: (T, T) => Boolean): List[T] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def merge(xs: List[T], ys: List[T]): List[T] =
      (xs, ys) match {
        case (xs, Nil) => xs
        case (Nil, ys) => ys
        case (x :: xs1, y :: ys1) =>
          if (lt(x, y)) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }

    val (first, second) = xs splitAt n
    merge(msort_parameterized(first)(lt), msort_parameterized(second)(lt))
  }
}

msort_parameterized(nums)((x: Int, y: Int) => x < y)

val fruits = List("pineapple", "banana", "orange", "apple")
msort_parameterized(fruits)((x: String, y: String) => x.compareTo(y) < 0)

// Instead of parmeterizing with the lt operation, we can parameterize with scala.math.Ordering instead
def msort_parameterizedWithOrdering[T](xs: List[T])(ord: Ordering[T]): List[T] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def merge(xs: List[T], ys: List[T]): List[T] =
      (xs, ys) match {
        case (xs, Nil) => xs
        case (Nil, ys) => ys
        case (x :: xs1, y :: ys1) =>
          if (ord.lt(x, y)) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }

    val (first, second) = xs splitAt n
    merge(msort_parameterizedWithOrdering(first)(ord), msort_parameterizedWithOrdering(second)(ord))
  }
}

msort_parameterizedWithOrdering(nums)(Ordering.Int)
msort_parameterizedWithOrdering(fruits)(Ordering.String)

def msort_parameterizedWithImplicitOrdering[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def merge(xs: List[T], ys: List[T]): List[T] =
      (xs, ys) match {
        case (xs, Nil) => xs
        case (Nil, ys) => ys
        case (x :: xs1, y :: ys1) =>
          if (ord.lt(x, y)) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }

    val (first, second) = xs splitAt n
    merge(msort_parameterizedWithImplicitOrdering(first), msort_parameterizedWithImplicitOrdering(second))
  }
}

msort_parameterizedWithImplicitOrdering(nums)
msort_parameterizedWithImplicitOrdering(fruits)

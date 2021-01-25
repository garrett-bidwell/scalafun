class Rational(x: Int, y: Int) {
  require( y != 0, "denominator must be nonzero")
  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  private val g = gcd(x, y)
  def numer = x/g
  def denom = y/g

  def add(other: Rational) =
    new Rational(
      numer * other.denom + other.numer * denom,
      denom * other.denom
    )

  def sub(other: Rational) = add(other.neg)
    /*
    new Rational(
      numer * other.denom - other.numer * denom,
      denom * other.denom
    )
    */

  def neg: Rational = new Rational(-numer, denom)

  def less(other: Rational) = numer * other.denom < other.numer * denom

  def max(other: Rational) = if (this.less(other)) other else this

  override def toString() = numer + "/" + denom
}

val x = new Rational(1, 2)
x.numer
x.denom
val y = new Rational(2, 3)
x.add(y)
y.sub(x)
x.neg
val a = new Rational(1, 3)
val b = new Rational(5, 7)
val c = new Rational(3, 2)
a.sub(b).sub(c)
b.add(b)
// gcd(70, 49) = gcd(49, 21) = gdb(21, 7) = gcd(7, 0) = 7
a.less(b)
a.max(b)
val strange = new Rational(1, 0)


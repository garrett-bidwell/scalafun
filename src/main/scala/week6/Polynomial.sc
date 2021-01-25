// a polynomial can be represented as a map from exponents to coefficients
// e.g. x^3 - 2x + 5 can be represented as
// Map(0 -> 5, 1 -> -2, 3 -> 1)
class Polynomial(val terms: Map[Int, Double]) {
  def + (other: Polynomial) = new Polynomial(terms ++ (other.terms map adjustCoeff))
  def adjustCoeff(term: (Int, Double)): (Int, Double) = {
    val (exp, coeff) = term
    terms get exp match {
      case Some(coeff1) => exp -> (coeff + coeff1)
      case None => exp -> coeff
    }
  }

  override def toString =
    (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff+"x^"+exp) mkString " + "
}

val p1 = new Polynomial(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
val p2 = new Polynomial(Map(0 -> 3.0, 3 -> 7.0))
p1 + p2

// we can simplify the logic in Polynomial by using default values
class Poly(terms0: Map[Int, Double]) {
  // we can simplify construction of polynomials with this constructor
  def this(bindings: (Int, Double)*) = this(bindings.toMap)
  val terms = terms0 withDefaultValue 0.0
  //def + (other: Poly) = new Poly(terms ++ (other.terms map adjustCoeff))
  // + implementation using foldLeft
  def + (other: Poly) = new Poly((other.terms foldLeft terms)(addTerm))
  def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
    val (exp, coeff) = term
    terms + (exp -> (coeff + terms(exp)))
  }
  def adjustCoeff(term: (Int, Double)): (Int, Double) = {
    val (exp, coeff) = term
    exp -> (coeff + terms(exp))
  }

  override def toString =
    (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff+"x^"+exp) mkString " + "
}

val poly1 = new Poly(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
poly1.terms(7)
val poly2 = new Poly(Map(0 -> 3.0, 3 -> 7.0))
poly1 + poly2
val poly3 = new Poly(1 -> 2.0, 3 -> 4.0, 5 -> 6.2)
val poly4 = new Poly(0 -> 3.0, 3 -> 7.0)

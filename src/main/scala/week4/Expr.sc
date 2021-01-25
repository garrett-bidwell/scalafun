trait Expr
case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr
case class Var(x: String) extends Expr

def show(e: Expr): String = e match {
  case Number(n) => n.toString
  case Sum(e1, e2) => show(e1) + " + " + show(e2)
  case Var(x: String) => x
  case Prod(e1, e2) =>
    if (e1.isInstanceOf[Sum] && e2.isInstanceOf[Sum]) "(" + show(e1) + ") * (" + show(e2) + ")"
    else if (e1.isInstanceOf[Sum]) "(" + show(e1) + ") * " + show(e2)
    else if (e2.isInstanceOf[Sum]) show(e1) + " * (" + show(e2) + ")"
    else show(e1) + " * " + show(e2)
}

show(Sum(Number(1), Number(44)))
show(Sum(Prod(Number(2), Var("x")), Var("y")))
show(Prod(Sum(Number(2), Var("x")), Var("y")))
show(Prod(Var("y"), Sum(Number(2), Var("x"))))
show(Prod(Sum(Number(2), Var("x")), Sum(Number(1), Var("y"))))

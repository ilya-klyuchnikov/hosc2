package hosc.lang.io

/*
 * This is an abstract syntax for the input/output language
 * HOSC deals with.
 * 
 * This language is separated in a sense from core language
 * and used for input/output, type checking 
 * rather that for supercompilation.
 */

abstract sealed class Expr

case class Var(name: String) extends Expr
case class Con(name: String, args: List[Expr]) extends Expr
case class Lam(v: Var, e: Expr) extends Expr
case class App(e1: Expr, e2: Expr) extends Expr
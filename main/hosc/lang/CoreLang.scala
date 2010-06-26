package hosc.lang

import Util._

/**
 * See http://hackage.haskell.org/trac/ghc/wiki/Commentary/Compiler/CoreSynType
 * for description of Haskell core language. 
 */
abstract sealed class Expr {
	/**
	 * Canonical size of expression 
	 */
	def size: Int
}

case class Var(name: String) extends Expr {
	val size = 1
}

case class Con(name: String, args: List[Expr]) extends Expr {
	val size = 1 + sum(args map {_.size})
}

case class Lam(v: Var, body: Expr) extends Expr {
	val size = 1 + body.size
}

case class App(e1: Expr, e2: Expr) extends Expr {
	val size = e1.size + e2.size
}

case class Case(sel: Expr, alts: List[Alt]) extends Expr {
	val size = sel.size + sum(alts map {1 + _.expr.size})
}

case class Let(binds: List[Bind], expr: Expr) extends Expr {
	val size = expr.size + sum(binds map {1 + _.expr.size})
}

// auxiliary data  
case class Bind(v: Var, expr: Expr)
case class Alt(pat: Pat, expr: Expr)
case class Pat(name: String, args: List[Var])
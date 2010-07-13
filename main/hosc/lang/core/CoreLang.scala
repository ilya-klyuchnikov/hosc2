package hosc.lang.core

import Util._

/*
 * This is an attempt to represent a subset of Haskell Language that is 
 * close to Haskell Core.
 *  
 * See:
 *   http://hackage.haskell.org/trac/ghc/wiki/Commentary/Compiler/CoreSynType
 *   http://hackage.haskell.org/package/haskell-src-1.0.1.3 
 */

case class Module(dataTypes: List[DataType], binds: List[Bind])

abstract sealed class Expr {
	def size: Int
}

abstract sealed class Var extends Expr {
	val size = 1
}

// free, global, bound variables
case class FVar(i: Int) extends Var
case class GVar(i: Int) extends Var {
	override def toString = "[" + i + "]"
}
case class BVar(i: Int) extends Var {
	override def toString = i.toString
}

case class Con(name: String, args: List[Expr], dtIndex: Int = -1, dcIndex: Int = -1) extends Expr {
	val size = 1 + sum(args map {_.size})
	override def toString = "(" + name + (args match {case Nil => ""; case _ => args.mkString(" ", " ","")}) + ")"
}

case class Lam(e: Expr) extends Expr {
	val size = 1 + e.size
	override def toString = "(\\" + e + ")"
}

case class App(e1: Expr, e2: Expr) extends Expr {
	val size = e1.size + e2.size
	override def toString = "(" + e1 + " " + e2 + ")"
}

case class Case(dataType: Int = -1, sel: Expr, alts: List[Expr]) extends Expr {
	val size = sel.size + sum(alts map {1 + _.size})
}

case class Let(binds: List[Bind], expr: Expr) extends Expr {
	val size = expr.size + sum(binds map {1 + _.expr.size})
}
  
case class Bind(v: Var, expr: Expr)

case class DataCon(name: String, arity: Int)
case class DataType(cons: List[DataCon])

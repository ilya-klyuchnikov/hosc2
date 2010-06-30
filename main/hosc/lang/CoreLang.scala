package hosc.lang

import Util._

/*
 * This is an attempt to represent a subset of Haskell Language that is 
 * close to Haskell Core.
 *  
 * See:
 *   http://hackage.haskell.org/trac/ghc/wiki/Commentary/Compiler/CoreSynType
 *   http://hackage.haskell.org/package/haskell-src-1.0.1.3 
 */

case class Module(dataDecls: List[DataDecl], binds: List[Bind])

abstract sealed class Expr {
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
  
case class Bind(v: Var, expr: Expr)
case class Alt(pat: Pat, expr: Expr)
case class Pat(name: String, args: List[Var])

sealed abstract class Type
case class TypeVar(name: String) extends Type
case class TypeCon(name: String, args: List[Type]) extends Type
case class TypeFun(from: Type, to: Type) extends Type

case class DataCon(name: String, args: List[Type])
case class DataDecl(name: String, args: List[TypeVar], cons: List[DataCon])

// "abstract bindings"
case class TypeSig(name: String, `type`: Type)
package hosc.lang.io

/*
 * This is an abstract syntax for the input/output language
 * HOSC deals with.
 * 
 * This language is separated in a sense from core language
 * and used for input/output, type checking 
 * rather that for supercompilation.
 */

abstract sealed class HsExpr

case class HsVar(name: String) extends HsExpr
case class HsCon(name: String, args: List[HsExpr]) extends HsExpr
case class HsLam(v: HsVar, e: HsExpr) extends HsExpr
case class HsApp(e1: HsExpr, e2: HsExpr) extends HsExpr

case class HsCase(sel: HsExpr, alts: List[HsAlt]) extends HsExpr

case class HsBind(v: HsVar, expr: HsExpr)
case class HsAlt(pat: HsPat, expr: HsExpr)
case class HsPat(name: String, args: List[HsVar])
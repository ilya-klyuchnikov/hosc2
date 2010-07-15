package hosc.lang.hs

/*
 * This is an abstract syntax for the input/output language
 * HOSC deals with.
 * 
 * This language is separated in a sense from core language
 * and used for input/output, type checking 
 * rather that for supercompilation.
 */

case class HsModule(dataDefs: List[HsDataDef], binds: List[HsBind])

abstract sealed class HsExpr

case class HsVar(name: String) extends HsExpr
case class HsCon(name: String, args: List[HsExpr]) extends HsExpr
case class HsLam(v: HsVar, e: HsExpr) extends HsExpr
case class HsApp(e1: HsExpr, e2: HsExpr) extends HsExpr

case class HsCase(sel: HsExpr, alts: List[HsAlt]) extends HsExpr
case class HsLet(binds: List[HsBind], expr: HsExpr) extends HsExpr

case class HsBind(v: HsVar, expr: HsExpr)
case class HsAlt(pat: HsPat, expr: HsExpr)
case class HsPat(name: String, args: List[HsVar])

sealed abstract class HsType
case class HsTypeVar(name: String) extends HsType
case class HsTypeCon(name: String, args: List[HsType]) extends HsType
case class HsTypeFun(from: HsType, to: HsType) extends HsType

case class HsDataCon(name: String, args: List[HsType])
case class HsDataDef(name: String, args: List[HsTypeVar], cons: List[HsDataCon])

// "abstract bindings"
case class TypeSig(name: String, `type`: HsType)
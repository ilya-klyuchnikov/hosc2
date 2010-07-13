package hosc.lang.core

import hosc.lang.io._

object DeBruijnConverter {
	Nil.lastIndexOf(null)
	
	def hs2core(hsMod: HsModule) = {
		val context = hsMod.binds map {_.v}
	}
	
	def removeNames(gCon: DeBruijnContext, bCon: DeBruijnContext, expr: HsExpr): Expr = expr match {
		case v@HsVar(_) => {
			val bInd = bCon.indexOf(v)
			if (bInd == -1) GVar(gCon.indexOf(v)) else BVar(bInd)
		}
		case HsLam(v, e) => Lam(removeNames(gCon, v :: bCon, e))
		case HsCon(n, args) => Con(n, args map {removeNames(gCon, bCon, _)})
		case HsApp(e1, e2) => App(removeNames(gCon, bCon, e1), removeNames(gCon, bCon, e2))
	}
}
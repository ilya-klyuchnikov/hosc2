package hosc.lang.core

import hosc.lang.io._

object DeBruijnConverter {
	Nil.lastIndexOf(null)
	
	def hs2core(hsMod: HsModule) = {
		val context = hsMod.binds map {_.v}
	}
	
	def removeNames(conMap: Map[String, (Int, Int)], gCon: DeBruijnContext, bCon: DeBruijnContext, fCon: DeBruijnContext, expr: HsExpr): Expr = {
		
		def remove(bCon: DeBruijnContext, expr: HsExpr): Expr = expr match {
			case v@HsVar(_) => {
				lazy val bI = bCon.indexOf(v)
				lazy val gI = gCon.indexOf(v)
				lazy val fI = fCon.indexOf(v)
				if (bI >= 0) { BVar(bI) } else if (gI >= 0){ GVar(gI) } else { FVar(fI) }
			}
			// context expansion
			case HsLam(v, e) => Lam(remove(v :: bCon, e))
			case HsCon(n, args) => {
				val (dtI, dcI) = conMap.getOrElse(n, (-1, -1))
				Con(n, args map {remove(bCon, _)}, dtI, dcI)
			}
			case HsApp(e1, e2) => App(remove(bCon, e1), remove(bCon, e2))
			// converting alternatives to lambdas
			case HsCase(sel, alts) => {
				val (dtI, _) = conMap.getOrElse(alts.head.pat.name, (-1, -1))
				val lambdas = alts map {case HsAlt(HsPat(n, xs), e) => xs.foldRight(e){HsLam}}
				val convAlts = lambdas map {remove(bCon, _)}
				val convSel = remove(bCon, sel)
				Case(dtI, convSel, convAlts)
			}
			case let@HsLet(_, _) => error("unexpected let: " + let)
		}
		
		remove(bCon, expr)
	}
}
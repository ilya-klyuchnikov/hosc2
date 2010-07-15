package hosc.lang.core

import hosc.lang.hs._

object DeBruijnConverter {
	
	def removeNames(conMap: Map[String, (Int, Int)], gCon: DeBruijnContext, bCon: ExtendedDeBruijnContext, fCon: DeBruijnContext, expr: HsExpr): Expr = {
		val conInfo = conMap.withDefaultValue((-1, -1))
		def remove(bCon: ExtendedDeBruijnContext, expr: HsExpr): Expr = expr match {
			
			case v@HsVar(_) => {
				val (bvs, is) = bCon
				lazy val bI = bvs.indexOf(v)
				lazy val gI = gCon.indexOf(v)
				lazy val fI = fCon.indexOf(v)
				
				if (bI >= 0) { BVar(bI, is(bI)) } else if (gI >= 0) { GVar(gI) } else { FVar(fI) }
			}
			
			case HsLam(v, e) => {
				val eBcon = expand(v, bCon)
				
				Lam(remove(eBcon, e))
			}
			
			case HsCon(n, args) => {
				val iBCon = inc(bCon)
				
				val (dtI, dcI) = conInfo(n)
				Con(n, args map {remove(iBCon, _)}, dtI, dcI)
			}
			
			case HsApp(e1, e2) => {
				val iBCon = inc(bCon)
				
				App(remove(iBCon, e1), remove(iBCon, e2))
			}
			
			// converting alternatives to lambdas
			case HsCase(sel, alts) => {
				val iBCon = inc(bCon)
				
				val (dtI, _) = conInfo(alts.head.pat.name)
				val lambdas = alts map {case HsAlt(HsPat(n, xs), e) => xs.foldRight(e){HsLam}}
				
				val convAlts = lambdas map {remove(iBCon, _)}
				val convSel = remove(iBCon, sel)
				
				Case(dtI, convSel, convAlts)
			}
			
			case let@HsLet(_, _) => error("unexpected let: " + let)
		}
		
		remove(bCon, expr)
	}
	
	private def inc(c: ExtendedDeBruijnContext) = 
		c match {case (vars, is) => (vars, is map {_ + 1})}
	
	private def expand(v: HsVar, c: ExtendedDeBruijnContext) = 
		c match {case (vars, is) => (v :: vars, 0 :: (is map {_ + 1}))}
}
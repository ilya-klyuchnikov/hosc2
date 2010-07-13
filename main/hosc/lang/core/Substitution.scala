package hosc.lang.core

object Substitution {
	// def applyRenaming [E <: Expr] (expr: E): E
	def rename (expr: Expr, r: Renaming) = 
		mapFV(expr, {v => r.getOrElse(v, v)})
	
	def sub(expr: Expr, s: Subst) = 
		mapFV(expr, {v => s.getOrElse(v, v)})
	
	// transforms free variables
	// assumes that bound variables do not shadow free variables
	def mapFV(expr: Expr, sub: (FVar => Expr)): Expr = {
		// ms = map substitution
		def ms(e: Expr): Expr = expr match {
			case v@FVar(_) => sub(v)
			case Con(n, es, dtI, dcI) => Con(n, es map ms, dtI, dcI)
			case Lam(e) => Lam(ms(e))
			case App(e1, e2) => App(ms(e1), ms(e2))
			case Case(dt, sel, alts) => Case(dt, ms(sel), alts map {ms})
			case Let(bs, e) => Let(bs map {case Bind(v, e) => Bind(v, ms(e))}, ms(e))
			case _ => e
		}
		
		ms(expr)
	}
	
	def mapE(expr: Expr, r: Replacement): Expr = {
		def walk(e: Expr): Expr = r.andThen(fallback)(e)
		
		def fallback(e: Expr): Expr = e match {
			case Con(n, es, dtI, dcI) => Con(n, es map walk, dtI, dcI)
			case Lam(e) => Lam(walk(e))
			case App(e1, e2) => App(e1, e2)
			case Case(dt, sel, alts) => Case(dt, walk(sel), alts map {walk})
			case Let(bs, e) => Let(bs map {case Bind(v, e) => Bind(v, walk(e))}, walk(e))
			case _ => e
		}
		
		walk(expr)
	}
	
}
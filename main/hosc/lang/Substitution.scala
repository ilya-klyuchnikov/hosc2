package hosc.lang

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
			case Con(n, es) => Con(n, es map ms)
			case Lam(e) => Lam(ms(e))
			case App(e1, e2) => App(ms(e1), ms(e2))
			case Case(sel, alts) => Case(ms(sel), alts map {case Alt(p, e) => Alt(p, ms(e))})
			case Let(bs, e) => Let(bs map {case Bind(v, e) => Bind(v, ms(e))}, ms(e))
			case _ => e
	    }
		ms(expr)
	}
	
	def mapE(expr: Expr, r: Replacement): Expr = {
		def walk(e: Expr): Expr = r.andThen(fallback)(e)
		
		def fallback(e: Expr): Expr = e match {
			case Con(n, es) => Con(n, es map walk)
			case Lam(e) => Lam(walk(e))
			case App(e1, e2) => App(e1, e2)
			case Case(sel, alts) => Case(walk(sel), alts map {case Alt(p, e) => Alt(p, walk(e))})
			case Let(bs, e) => Let(bs map {case Bind(v, e) => Bind(v, walk(e))}, walk(e))
			case _ => e
		}
		
		walk(expr)
	}
	
}
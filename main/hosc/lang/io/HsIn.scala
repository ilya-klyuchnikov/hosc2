package hosc.lang.io

object HsPostProcessor {
	
	def walk(mod: HsModule) = {
		val fixedBs = mod.binds map {case HsBind(v, e) => HsBind(v, cons(e))} 
		HsModule(mod.dataDefs, fixedBs)
	}
	
	// it just 
	def cons(e: HsExpr): HsExpr = e match {
		case HsVar(n) if uId_?(n) => HsCon(n, List())
		case HsLam(v, e1) => HsLam(v, cons(e1))
		case HsApp(e1, e2) => cons(e1) match {
			case HsCon(n, args) => HsCon(n, args ++ List(cons(e2)))
			case e => HsApp(e, cons(e2))
		}
		case HsCase(sel, alts) => HsCase(cons(sel), alts map {case HsAlt(p, e1) => HsAlt(p, cons(e1))})
		case HsLet(bs, e1) => HsLet(bs map {case HsBind(v, x) => HsBind(v, cons(x))}, cons(e1))
		case _ => e
	}
	
	def uId_?(id: String) = id.head.isUpper
	def lId_?(id: String) = id.head.isLower
}

import scala.io.Source
import scala.util.parsing.input.StreamReader

object HsIn {
	def readHsModule(path: String): HsModule = {
		val reader = StreamReader(Source.fromFile(path).reader)
		val parseResult = HsParsers.parse(HsParsers.module)(reader)
		val module = parseResult.get
		HsPostProcessor.walk(module)
	}
}
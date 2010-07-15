package hosc.lang.hs

import scala.io.Source
import scala.util.parsing.input.StreamReader

/**
 * This object provides convenience methods to get
 * an abstract tree of hs language from different sources.
 */
object HsSource {
	
	def readHsModule(path: String): HsModule = {
		val reader = StreamReader(Source.fromFile(path).reader)
		val parseResult = HsParsers.parse(HsParsers.module)(reader)
		val module = parseResult.get
		walkAllFixes(module)
	}
	
	def walkAllFixes(mod: HsModule) = {
		val fixedConsBs = mod.binds map {case HsBind(v, e) => HsBind(v, cons(e))}
		val consMap = consInfo(mod)
		val fixedCazeBs = fixedConsBs map {case HsBind(v, e) => HsBind(v, canonizeCase(consMap, e))}
		HsModule(mod.dataDefs, fixedCazeBs)
	}
	
	def walkConFixes(mod: HsModule) = {
		val fixedBs = mod.binds map {case HsBind(v, e) => HsBind(v, cons(e))} 
		HsModule(mod.dataDefs, fixedBs)
	}
	
	// converts constructors parsed as applications
	// into real applications
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
	
	def dataTypeIndex(module: HsModule, dt: HsDataDef): Int = 
		module.dataDefs.indexWhere(_ == dt)
	
	def consInfo(module: HsModule): Map[String, (Int, Int)] = {
		val indexed = module.dataDefs map {_.cons.zipWithIndex} zipWithIndex
		val elems = List.concat(indexed map {case (d, i1) => d map {case (c, i2) => (c.name, (i1, i2))}}: _*)
		Map(elems: _*)
	}
	
	def canonizeCase(consInfo: Map[String, (Int, Int)], expr: HsExpr): HsExpr = expr match {
		case HsLam(v, e) => HsLam(v, canonizeCase(consInfo, e))
		case HsApp(e1, e2) => HsApp(canonizeCase(consInfo, e1), canonizeCase(consInfo, e2))
		case HsLet(bs, e1) => HsLet(bs map {case HsBind(v, x) => HsBind(v, canonizeCase(consInfo, x))}, canonizeCase(consInfo, e1))
		case HsCase(sel, alts) => {
			val canSel = canonizeCase(consInfo, sel)
			val sortedAlts = alts.sortBy{x => consInfo(x.pat.name)._2}
			val canAlts = sortedAlts map {case HsAlt(p, e1) => HsAlt(p, canonizeCase(consInfo, e1))}
			HsCase(canSel, canAlts)
		}
		case HsCon(name, args) => HsCon(name, args map {canonizeCase(consInfo, _)})
		case _ => expr
	}
}
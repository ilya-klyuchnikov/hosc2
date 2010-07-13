package hosc.lang.io

import scala.io.Source
import scala.util.parsing.input.StreamReader

object HsIn {
	
	def readHsModule(path: String): HsModule = {
		val reader = StreamReader(Source.fromFile(path).reader)
		val parseResult = HsParsers.parse(HsParsers.module)(reader)
		val module = parseResult.get
		walkAllFixes(module)
	}
	
	def walkAllFixes(mod: HsModule) = {
		val fixedConsBs = mod.binds map {case HsBind(v, e) => HsBind(v, cons(e))}
		val fixedCazeBs = fixedConsBs map {case HsBind(v, e) => HsBind(v, canonizeCase(mod, e))}
		HsModule(mod.dataDefs, fixedConsBs)
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
		
	def conInfo(module: HsModule, conName: String): (Int, Int) = {
		val dataDef = module.dataDefs.find{_.cons.exists{_.name == conName}}.get
		val dataDefIndex = module.dataDefs.indexOf(dataDef)
		val dataConIndex = dataDef.cons.indexWhere(_.name == conName)
		(dataDefIndex, dataConIndex)
	}
	
	def canonizeCase(module: HsModule, expr: HsExpr): HsExpr = expr match {
		case HsLam(v, e) => HsLam(v, canonizeCase(module, e))
		case HsApp(e1, e2) => HsApp(canonizeCase(module, e1), canonizeCase(module, e2))
		case HsLet(bs, e1) => HsLet(bs map {case HsBind(v, x) => HsBind(v, canonizeCase(module, x))}, canonizeCase(module, e1))
		case HsCase(sel, alts) => {
			val canSel = canonizeCase(module, sel)
			val sortedAlts = alts.sortBy{x => conInfo(module, x.pat.name)._2}
			val canAlts = sortedAlts map {case HsAlt(p, e1) => HsAlt(p, canonizeCase(module, e1))}
			HsCase(canSel, canAlts)
		}
		case HsCon(name, args) => HsCon(name, args map {canonizeCase(module, _)})
		case _ => expr
	}
}
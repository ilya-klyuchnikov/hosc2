package hosc.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import hosc.lang.core._
import hosc.lang.core.DeBruijnConverter

class DeBruijnConverterSpec extends FunSuite with ShouldMatchers with Util {
	implicit def int2BVar(i: Int) = BVar(i)
	
	// see Exercise 6.1.1 in TAPL
	test("Simple converting") {
		convertExpr("""\s z -> z""") should equal
			{Lam(Lam(0))}
		
		convertExpr("""\s z -> s (s z)""") should equal
			{Lam( Lam( App(1, App(1, 0)) ) )}
		
		convertExpr("""\x -> S (S x)""") should equal
			{Lam( Con("S", List(Con("S", List(0)))) )}
		
		// plus
		convertExpr("""\m n s z -> m s (n s z)""") should equal
			{Lam(Lam(Lam(Lam( App(App(3, 1), App(App(2, 1), 0)) ))))}
		
		// fix
		convertExpr("""\f -> (\x -> f (\y -> (x x) y)) (\x -> f (\y -> (x x) y)) """) should equal
			{Lam( App (Lam (App(1, Lam (App(App(1, 1), 0)))), Lam (App(1, Lam (App(App(1, 1), 0))))))}
		
		// foo
		convertExpr("""(\x -> (\x -> x)) (\x -> x)""") should equal
			{App(Lam(Lam(0)), Lam(0))}
	}
	
	def convertExpr(in: String) = {
		val expr = parseExpr(in)
		DeBruijnConverter.removeNames(Map(), Nil, Nil, Nil, expr)
	}
}
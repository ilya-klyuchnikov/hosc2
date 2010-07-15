package hosc.test

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

import hosc.lang.core._
import hosc.lang.core.DeBruijnConverter

@RunWith(classOf[JUnitRunner])
class DeBruijnConverterSpec extends FunSuite with ShouldMatchers with Util {
	
	// see Exercise 6.1.1 in TAPL
	// exercises are extended to extended de Bruijn indices
	test("Simple converting") {
		// λλ.(0,0)
		convertExpr("""\s z -> z""") should equal
			{Lam(Lam(BVar(0, 0)))}
		
		//λλ.(1,1)
		convertExpr("""\s z -> s""") should equal
			{Lam(Lam(BVar(1, 1)))}
		
		convertExpr("""\s z -> s (s z)""") should equal
			{Lam( Lam( App(BVar(1, 2), App(BVar(1, 3), BVar(0, 2))) ) )}
		
		convertExpr("""\x -> S (S x)""") should equal
			{Lam( Con("S", List(Con("S", List(BVar(0, 2))))) )}
		
		// plus
		convertExpr("""\m n s z -> m s (n s z)""") should equal
			{Lam(Lam(Lam(Lam( App(App(BVar(3, 5), BVar(1, 3)), App(App(BVar(2, 5), BVar(1, 4)), BVar(0, 2))) ))))}
		
		// fix
		convertExpr("""\f -> (\x -> f (\y -> (x x) y)) (\x -> f (\y -> (x x) y)) """) should equal
			{Lam( App (Lam (App(BVar(1, 3), Lam (App(App(BVar(1, 4), BVar(1, 4)), BVar(0, 1))))), 
					Lam (App(BVar(1, 3), Lam (App(App(BVar(1, 4), BVar(1, 4)), BVar(0, 1)))))))}
		
		// foo
		convertExpr("""(\x -> (\x -> x)) (\x -> x)""") should equal
			{App(Lam(Lam(BVar(0, 0))), Lam(BVar(0, 0)))}
	}
	
	def convertExpr(in: String) = {
		val expr = parseExpr(in)
		DeBruijnConverter.removeNames(Map(), Nil, (Nil, Nil), Nil, expr)
	}
}
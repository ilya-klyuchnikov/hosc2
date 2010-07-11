package hosc.test

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import scala.util.parsing.input.{CharArrayReader, Reader}

import hosc.lang.io.{HsVar => V, HsApp => A, HsLam => L, HsCase => Cs, 
	HsAlt => Al, HsPat => P, HsBind => B, HsLet => Let, _}

@RunWith(classOf[JUnitRunner])
class HsParsersSpec extends FunSuite with ShouldMatchers {
	implicit def symbol2var(s: Symbol) = V(s.name)
	
	test("Simple inputs") {
		parseRaw("a") should equal {V("a")}
		
		parseRaw("(a)") should equal {V("a")}
		
		parseRaw("a b") should equal {A('a , 'b)}
		
		parseRaw("x y z") should equal {A(A('x, 'y), 'z)}
		
		parseRaw("(x y) z") should equal {A(A('x, 'y), 'z)}
		
		parseRaw("x (y z)") should equal {A('x, A('y, 'z))}
		
		parseRaw("(a b) (c d)") should equal {A (A('a, 'b), A('c, 'd))}
		
		parseRaw("\\x y -> x y") should equal {L('x, L('y, A('x, 'y)))}
		
		parseRaw("case x of {Nil -> Nil;}") should equal 
			{Cs('x, List( Al(P("Nil", Nil), 'Nil) ))}
		
		parseRaw{"let x = y; z = t; in x t"} should equal 
			{Let(List(B('x, 'y), B('z, 't)), A('x, 't))}
		
		parseRaw{"let x = y; in let z = t; in x t"} should equal 
			{Let(List(B('x, 'y)), Let(List(B('z, 't)), A('x, 't)))}
		
	}
	
	import hosc.lang.io.HsParsers._
	def parseRaw(in: String) = {
		val res = parse(expr)(new CharArrayReader(in.toCharArray))
		res.get
	}
}
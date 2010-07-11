package hosc.test

import org.junit.runner.RunWith
import org.scalatest.{FlatSpec, FunSuite}
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import scala.util.parsing.input.{CharArrayReader, Reader}

import hosc.lang.io.{HsVar => V, HsApp => A, HsLam => L, _}

@RunWith(classOf[JUnitRunner])
class HsParsersSpec extends FunSuite with ShouldMatchers {
	test("Simple inputs") {
		parseRaw("a") should equal (V("a"))
		
		parseRaw("(a)") should equal (V("a"))
		
		parseRaw("a b") should equal (A(V("a"),V("b")))
		
		parseRaw("x y z") should equal (A(A(V("x"), V("y")), V("z")))
		
		parseRaw("(x y) z") should equal (A(A(V("x"), V("y")), V("z")))
		
		parseRaw("x (y z)") should equal (A(V("x"), A(V("y"), V("z"))))
		
		parseRaw("(a b) (c d)") should equal (A( A(V("a"), V("b")), A(V("c"), V("d")) ))
		
		parseRaw("\\x y -> x y") should equal (L(V("x"), L(V("y"), A(V("x"), V("y")))))
	}
	
	import hosc.lang.io.HsParsers._
	def parseRaw(in: String) = {
		val res = parse(expr)(new CharArrayReader(in.toCharArray))
		res.get
	}
}
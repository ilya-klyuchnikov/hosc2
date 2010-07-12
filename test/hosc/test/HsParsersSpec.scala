package hosc.test

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import scala.util.parsing.input.{CharArrayReader, Reader}

import hosc.lang.io.{HsVar => V, HsApp => A, HsLam => L, HsCase => Cs, 
	HsAlt => Al, HsPat => P, HsBind => B, HsLet => Let, HsCon => C, 
	HsTypeVar => TV, HsTypeCon => TC, HsTypeFun => TF, _}

@RunWith(classOf[JUnitRunner])
class HsParsersSpec extends FunSuite with ShouldMatchers {
	implicit def symbol2var(s: Symbol) = V(s.name)
	implicit def symbol2tvar(s: Symbol) = TV(s.name)
	
	test("Base expression parsing") {
		parseExprRaw("a") should equal {V("a")}
		
		parseExprRaw("A") should equal {V("A")}
		
		parseExprRaw("(a)") should equal {V("a")}
		
		parseExprRaw("a b") should equal {A('a , 'b)}
		
		parseExprRaw("x y z") should equal {A(A('x, 'y), 'z)}
		
		parseExprRaw("(x y) z") should equal {A(A('x, 'y), 'z)}
		
		parseExprRaw("x (y z)") should equal {A('x, A('y, 'z))}
		
		parseExprRaw("(a b) (c d)") should equal {A (A('a, 'b), A('c, 'd))}
		
		parseExprRaw("\\x y -> x y") should equal {L('x, L('y, A('x, 'y)))}
		
		parseExprRaw("case x of {Nil -> Nil;}") should equal 
			{Cs('x, List( Al(P("Nil", Nil), 'Nil) ))}
		
		parseExprRaw{"let x = y; z = t; in x t"} should equal 
			{Let(List(B('x, 'y), B('z, 't)), A('x, 't))}
		
		parseExprRaw{"let x = y; in let z = t; in x t"} should equal 
			{Let(List(B('x, 'y)), Let(List(B('z, 't)), A('x, 't)))}
	}
	
	test("Base expression parsing + constructor postprocessing") {
		parseExprPost("A") should equal 
			{C("A", Nil)}
		
		parseExprPost("A b") should equal 
        	{C("A", 'b :: Nil)}
		
		parseExprPost("(A b)") should equal 
        	{C("A", 'b :: Nil)}
		
		parseExprPost("A (b)") should equal 
        	{C("A", 'b :: Nil)}
		
		parseExprPost("(A) b") should equal 
        	{C("A", 'b :: Nil)}
		
		parseExprPost("A B C") should equal 
        	{C("A", C("B", Nil) :: C("C", Nil) :: Nil)}
		
		parseExprPost("A (B C)") should equal 
        	{C("A", C("B", C("C", Nil) :: Nil) :: Nil)}
		
		parseExprPost("(A B) C") should equal 
        	{C("A", C("B", Nil) :: C("C", Nil) :: Nil)}
		
		parseExprPost("Data1 (Data2 x y) z") should equal 
        	{C("Data1", C("Data2", V("x") :: V("y") :: Nil) :: V("z") :: Nil)}
		
		parseExprPost("case x of {Nil -> Nil;}") should equal 
			{Cs('x, List( Al(P("Nil", Nil), C("Nil", Nil)) ))}
		
		parseExprPost("a Cons b c") should equal 
			{A(A(A(V("a"), C("Cons", Nil)), V("b")), V("c"))}
		
		parseExprPost("(A V) e") should equal 
			{C("A", C("V", Nil) :: V("e") :: Nil)}
		
		parseExprPost("x A y") should equal 
			{A(A(V("x"), C("A", Nil)), V("y"))}
		
		parseExprPost("x y z v") should equal 
			{A(A(A(V("x"), V("y")), V("z")), V("v"))}
		
		parseExprPost("(x y) z v") should equal 
			{A(A(A(V("x"), V("y")), V("z")), V("v"))}
		
		parseExprPost("(x y) (z v)") should equal 
			{A(A(V("x"), V("y")), A(V("z"), V("v")))}
		
		parseExprPost("x Y z V") should equal 
			{A(A(A(V("x"), C("Y", Nil)), V("z")), C("V", Nil))}
		
		parseExprPost("(x Y) z V") should equal 
			{A(A(A(V("x"), C("Y", Nil)), V("z")), C("V", Nil))}
		
		parseExprPost("(x Y) (z V)") should equal 
			{A(A(V("x"), C("Y", Nil)), A(V("z"), C("V", Nil)))}
		
		parseExprPost("x (A y)") should equal 
			{A(V("x"), C("A", V("y") :: Nil))}
		
		parseExprPost("""\x -> (\y -> (Cons x y))""") should equal 
			{L(V("x"), L(V("y"), C("Cons", List(V("x"), V("y")))))}
		
		parseExprPost("""\x -> (\y -> Cons x y)""") should equal 
			{L(V("x"), L(V("y"), C("Cons", List(V("x"), V("y")))))}
		
		parseExprPost("""\x -> \y -> Cons x y""") should equal 
			{L(V("x"), L(V("y"), C("Cons", List(V("x"), V("y")))))}
		
		parseExprPost("""\x y -> Cons x y""") should equal 
			{L(V("x"), L(V("y"), C("Cons", List(V("x"), V("y")))))}
	}
	
	test("base type parsing") {
		parseType("TC1 TC2 a TC2 a") should equal
			{TC("TC1", TC("TC2", Nil) :: TV("a") :: TC("TC2", Nil) :: TV("a") :: Nil)}
	}
	
	import hosc.lang.io.HsParsers._
	import hosc.lang.io.HsPostProcessor._
	
	def parseExprRaw(in: String) = {
		val res = parse(expr)(new CharArrayReader(in.toCharArray))
		res.get
	}
	
	def parseType(in: String) = {
		val res = parse(tCon)(new CharArrayReader(in.toCharArray))
		res.get
	}
	
	def parseExprPost(in: String) = {
		cons(parseExprRaw(in))
	}
}
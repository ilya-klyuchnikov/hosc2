package hosc.test

import hosc.lang.io.HsParsers._
import hosc.lang.io.HsPostProcessor._
import scala.util.parsing.input.{CharArrayReader, Reader}

trait Util {
	def parseExprRaw(in: String) = {
		val res = parse(expr)(new CharArrayReader(in.toCharArray))
		res.get
	}
	
	def parseType(in: String) = {
		val res = parse(`type`)(new CharArrayReader(in.toCharArray))
		res.get
	}
	
	def parseExpr(in: String) = {
		cons(parseExprRaw(in))
	}
	
	def parseModule(in: String) = {
		val res = parse(module)(new CharArrayReader(in.toCharArray))
		walk(res.get)
	}
}
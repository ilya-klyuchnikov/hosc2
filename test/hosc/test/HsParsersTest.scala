package hosc.test

import org.junit.Test
import scala.util.parsing.input.{CharArrayReader, Reader}

import hosc.lang.io._

class HsParsersTest {
	@Test
	def simple1: Unit = {
		testSimple("a b c d")
	}
	
	def testSimple(in: String) = {
		val res = HsParsers.parseExpr(new CharArrayReader(in.toCharArray))
		println(in)
		println(res)
		println("***\n")
	}
}
package hosc.lang.io

import scala.util.parsing.input.{Positional, Reader}
import scala.util.parsing.combinator.token.StdTokens
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.{ImplicitConversions, Parsers, PackratParsers}

object HsParsers extends HsTokenParsers with PackratParsers with ImplicitConversions {
	lexical.delimiters += ("(", ")", ",", "=", ";", "{", "}", "::", "|", "->", "\\", "[", "]", "=>")
	lexical.reserved += ("case", "of", "where", "data", "let", "in")
	 
	lazy val expr: PackratParser[Expr] = chainl1(head, arg, success(App(_, _)))
	lazy val lam:  PackratParser[Lam] = "\\" ~> vrb ~ expr ^^ Lam
	lazy val vrb:  PackratParser[Var] = lId ^^ Var
	
	lazy val head = vrb | lam | "(" ~> expr <~ ")"
	lazy val arg = head | "(" ~> expr <~ ")"
	
	def parseExpr(r: Reader[Char]) = phrase(expr)(new PackratReader(new lexical.Scanner(r)))
}

class HsTokenParsers extends StdTokenParsers {
	type Tokens = HsTokens
	val lexical = new HsLexical
	def uId = elem("identifier", _.isInstanceOf[lexical.UId]) ^^ (_.chars)
	def lId = elem("identifier", _.isInstanceOf[lexical.LId]) ^^ (_.chars)  
}

trait HsTokens extends StdTokens {
	case class LId(chars: String) extends Token
	case class UId(chars: String) extends Token
}

import scala.util.parsing.input.CharArrayReader.EofCh
class HsLexical extends StdLexical with HsTokens { s: Parsers => 
	implicit def flatten[L](l: String => L) = (p: ~[s.Elem, List[s.Elem]]) => l(p._1 :: p._2 mkString)
	override def token = uLetter ~ rep(letter | digit) ^^ UId | lLetter ~ rep(letter | digit) ^^ LId | super.token
	def uLetter = elem("upper-letter", _.isUpper)
	def lLetter = elem("lower-letter", _.isLower)
	override protected def comment = '-' ~ '}'  ^^ { _ => ' '  } | chrExcept(EofCh) ~ comment
	override def whitespace = 
		rep(whitespaceChar | '{' ~ '-' ~ comment | '-' ~ '-' ~ rep(chrExcept(EofCh, '\n')) | '{' ~ '-' ~ failure("unclosed comment"))
}
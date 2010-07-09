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
	lazy val vrb:  PackratParser[Var] = ident ^^ Var
	
	lazy val head = vrb | lam | "(" ~> expr <~ ")"
	lazy val arg = head | "(" ~> expr <~ ")"
	
	def parseExpr(r: Reader[Char]) = phrase(expr)(new PackratReader(new lexical.Scanner(r)))
}

class HsTokenParsers extends StdTokenParsers {
	type Tokens = StdTokens
	val lexical = new HsLexical
}

import scala.util.parsing.input.CharArrayReader.EofCh
class HsLexical extends StdLexical { s: Parsers => 
	override protected def comment = '-' ~ '}'  ^^ { _ => ' '  } | chrExcept(EofCh) ~ comment
	override def whitespace = 
		rep(whitespaceChar | '{' ~ '-' ~ comment | '-' ~ '-' ~ rep(chrExcept(EofCh, '\n')) | '{' ~ '-' ~ failure("unclosed comment"))
}
package hosc.lang.io

import scala.util.parsing.input.{Positional, Reader}
import scala.util.parsing.combinator.token.StdTokens
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.{ImplicitConversions, Parsers, PackratParsers}

object HsParsers extends HsTokenParsers with PackratParsers with ImplicitConversions {
	lexical.delimiters += ("(", ")", ",", "=", ";", "{", "}", "::", "|", "->", "\\", "[", "]", "=>")
	lexical.reserved += ("case", "of", "where", "data", "let", "in")
	
	lazy val expr: PackratParser[HsExpr] = chainl1(head, arg, success(HsApp(_, _)))
	lazy val lam:  PackratParser[HsExpr] = ("\\" ~> (vrb+)) ~ ("->" ~> expr) ^^ {case vs ~ e => vs.foldRight(e) {HsLam(_, _)}}
	lazy val vrb:  PackratParser[HsVar] = ident ^^ HsVar
	lazy val head = vrb | lam | caze | "(" ~> expr <~ ")"
	lazy val arg = head | "(" ~> expr <~ ")"
	lazy val caze = ("case" ~> expr) ~ ("of" ~> "{" ~> (alt*) <~ "}") ^^ HsCase 
	
	lazy val bind = (vrb <~ "=") ~ (expr <~ ";") ^^ HsBind
	lazy val pat = ident ~ (vrb*) ^^ HsPat
	lazy val alt = pat ~ ("->" ~> expr <~ ";") ^^ HsAlt
	lazy val module = (bind*) ^^ HsModule
	
	def parse[T](p: Parser[T])(r: Reader[Char]): ParseResult[T] = 
		phrase(p)(new PackratReader(new lexical.Scanner(r)))
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
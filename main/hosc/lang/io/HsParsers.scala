package hosc.lang.io

import scala.util.parsing.input.{Positional, Reader}
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.combinator.{ImplicitConversions, Parsers, PackratParsers}

object HsParsers extends StandardTokenParsers with PackratParsers with ImplicitConversions {
	override val lexical = new HsLexical
	
	lexical.delimiters += ("(", ")", ",", "=", ";", "{", "}", "::", "|", "->", "\\", "[", "]", "=>")
	lexical.reserved += ("case", "of", "where", "data", "let", "in")
	
	lazy val module = (dataConDef*) ~ (bind*) ^^ HsModule
	
	lazy val expr: PackratParser[HsExpr] = rep1(elem) ^^ {_.reduceLeft{HsApp}}
	lazy val lam  = ("\\" ~> (vrb+)) ~ ("->" ~> expr) ^^ {case vs ~ e => vs.foldRight {e} {HsLam}}
	lazy val vrb  = ident ^^ HsVar
	lazy val caze = ("case" ~> expr) ~ ("of" ~> "{" ~> (alt*) <~ "}") ^^ HsCase 
	lazy val let  = ("let" ~> (bind+)) ~ ("in" ~> expr) ^^ HsLet
	lazy val elem = vrb | lam | caze | let | "(" ~> expr <~ ")"
	
	lazy val bind = (vrb <~ "=") ~ (expr <~ ";") ^^ HsBind
	lazy val pat = ident ~ (vrb*) ^^ HsPat
	lazy val alt = pat ~ ("->" ~> expr <~ ";") ^^ HsAlt
	
	lazy val dataConDef = ("data" ~> ident) ~ (tVrb*) ~ ("=" ~> rep1sep(dCon, "|") <~ ";") ^^ HsDataDef
	
	lazy val `type`: PackratParser[HsType] = rep1sep(tElem, "->") ^^ {_.reduceRight{HsTypeFun}}
	lazy val tVrb = ident ^^ HsTypeVar
	lazy val tCon = ident ~ (`type`*) ^^ HsTypeCon
	lazy val dCon = ident ~ (`type`*) ^^ HsDataCon
	lazy val tElem = tVrb | tCon | "(" ~> `type` <~ ")"
	
	def parse[T](p: Parser[T])(r: Reader[Char]) = phrase(p)(new PackratReader(new lexical.Scanner(r)))
}

import scala.util.parsing.input.CharArrayReader.EofCh
class HsLexical extends StdLexical { 
	override protected def comment = '-' ~ '}'  ^^ { _ => ' '  } | chrExcept(EofCh) ~ comment
	override def whitespace = 
		rep(whitespaceChar | '{' ~ '-' ~ comment | '-' ~ '-' ~ rep(chrExcept(EofCh, '\n')) | '{' ~ '-' ~ failure("unclosed comment"))
}
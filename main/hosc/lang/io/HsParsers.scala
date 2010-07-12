package hosc.lang.io

import scala.util.parsing.input.{Positional, Reader}
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.combinator.{ImplicitConversions, Parsers, PackratParsers}

object HsParsers extends StandardTokenParsers with ImplicitConversions {
	override val lexical = new HsLexical	
	lexical.delimiters += ("(", ")", ",", "=", ";", "{", "}", "::", "|", "->", "\\", "[", "]", "=>")
	lexical.reserved += ("case", "of", "where", "data", "let", "in")
	
	def module = (dataConDef*) ~ (bind*) ^^ HsModule
	
	def expr: Parser[HsExpr] = rep1(elem) ^^ {_.reduceLeft{HsApp}}
	def lam  = ("\\" ~> (vrb+)) ~ ("->" ~> expr) ^^ {case vs ~ e => vs.foldRight {e} {HsLam}}
	def vrb  = ident ^^ HsVar
	def caze = ("case" ~> expr) ~ ("of" ~> "{" ~> (alt*) <~ "}") ^^ HsCase 
	def let  = ("let" ~> (bind+)) ~ ("in" ~> expr) ^^ HsLet
	def elem = vrb | lam | caze | let | "(" ~> expr <~ ")"
	def bind = (vrb <~ "=") ~ (expr <~ ";") ^^ HsBind
	def pat = uId ~ (vrb*) ^^ HsPat
	def alt = pat ~ ("->" ~> expr <~ ";") ^^ HsAlt
	
	def dataConDef = ("data" ~> ident) ~ (tVrb*) ~ ("=" ~> rep1sep(dCon, "|") <~ ";") ^^ HsDataDef
	def `type`: Parser[HsType] = rep1sep(tElem, "->") ^^ {_.reduceRight{HsTypeFun}}
	def tVrb = lId ^^ HsTypeVar
	def tArg = uId ^^ {HsTypeCon(_, Nil)} | tVrb | "(" ~> `type` <~ ")"
	def tCon = uId ~ (tArg*) ^^ HsTypeCon
	def tElem = tVrb | tCon | "(" ~> `type` <~ ")"
	def dCon = uId ~ (`type`*) ^^ HsDataCon
	def parse[T](p: Parser[T])(r: Reader[Char]) = phrase(p)(new lexical.Scanner(r))
	
	def uId = ident ^? {case x if x.head.isUpper => x}
	def lId = ident ^? {case x if x.head.isLower => x}
}

import scala.util.parsing.input.CharArrayReader.EofCh
class HsLexical extends StdLexical { 
	override protected def comment = '-' ~ '}'  ^^ { _ => ' '  } | chrExcept(EofCh) ~ comment
	override def whitespace = 
		rep(whitespaceChar | '{' ~ '-' ~ comment | '-' ~ '-' ~ rep(chrExcept(EofCh, '\n')) | '{' ~ '-' ~ failure("unclosed comment"))
}
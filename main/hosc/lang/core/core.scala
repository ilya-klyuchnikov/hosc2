package hosc.lang

import hosc.lang.io._

package object core {
	type Subst = Map[FVar, Expr]
	type Renaming = Map[FVar, Var]
	type Replacement = Map[Expr, Expr]
	
	type DeBruijnContext = List[HsVar]
	type ExtendedDeBruijnContext = (List[HsVar], List[Int])	
	type DataConMeta = Map[String, (Int, Int)]
}
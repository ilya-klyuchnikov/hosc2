package hosc.lang

package object core {
  type Subst = Map[FVar, Expr]
  type Renaming = Map[FVar, Var]
  type Replacement = Map[Expr, Expr]
}
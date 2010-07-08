package hosc

package object lang {
  type Subst = Map[FVar, Expr]
  type Renaming = Map[FVar, Var]
  type Replacement = Map[Expr, Expr]
}
package semanticchecking

import ir._
import symboltable.SymbolTable

object LitIntChecker extends SemanticChecker[Unit] {

  // checked: 4, 22

  type T = Unit

  def initCheckerState: T = Unit

  def checkBound(node: Ir, value: BigInt, t: T): Result[T] =
    if (value < -BigInt("9223372036854775808")) Result.Error(SemanticError(node.getSource, "The integer is too small: " + value.toString()), t)
    else if (value > BigInt("9223372036854775807")) Result.Error(SemanticError(node.getSource, "The integer is too large: " + value.toString()), t)
    else Result.Good(t)

  def enter(node: Ir, st: SymbolTable, t: T): Result[T] = node match {
    case VarDecl.IDArrayDecl(_, size) =>
      if (size <= 0) Result.Error(SemanticError(node.getSource, "The size in an array declaration must be a positive integer; found " + size), t)
      else checkBound(node, size, t)
    case Stmt.For(_, _, _, step, _) =>
      if (step <= 0) Result.Error(SemanticError(node.getSource, "The step size of a for loop must be a positive integer; found " + step), t)
      else checkBound(node, step, t)
    case Expr.LitInt(value) => checkBound(node, value, t)
    case _ => Result.Good(t)
  }

  def leave(node: Ir, st: SymbolTable, t: T): Result[T] = Result.Good(t)

}

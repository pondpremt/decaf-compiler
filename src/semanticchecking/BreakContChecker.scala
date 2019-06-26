package semanticchecking

import ir.{Ir, Stmt}
import symboltable.SymbolTable

object BreakContChecker extends SemanticChecker[List[Boolean]] {

  //Checked: 23

  type T = List[Boolean]

  def initCheckerState: T = List(false)

  def enter(node: Ir, st: SymbolTable, t: T): Result[T] = node match {
    case _: Stmt.For | _: Stmt.While => Result.Good(true :: t)
    case Stmt.Break() | Stmt.Continue() =>
      if (t.head) Result.Good(t)
      else Result.Error(SemanticError(node.getSource, "All break and continue statements must be contained within the body of a for or a while"), t)
    case _ => Result.Good(t)
  }

  def leave(node: Ir, st: SymbolTable, t: T): Result[T] = node match {
    case _: Stmt.For | _: Stmt.While => Result.Good(t.tail)
    case _ => Result.Good(t)
  }

}

package semanticchecking

import ir._
import symboltable.SymbolTable

object MustReturnChecker extends SemanticChecker[List[Boolean]] {

  // Currently not used

  // Each statement pushes true if it definitely returns and false otherwise.
  // Each block pushes true if it definitely returns and false otherwise.
  type T = List[Boolean]

  def initCheckerState: T = Nil

  def enter(node: Ir, st: SymbolTable, t: T): Result[T] = Result.Good(t)

  def leave(node: Ir, st: SymbolTable, t: T): Result[T] = node match {
    case Stmt.Return(_) => Result.Good(true :: t)
    case Stmt.Cond(_, _, None) => Result.Good(false :: t.tail)
    case Stmt.Cond(_, _, Some(_)) => Result.Good((t.head && t(1)) :: t.drop(2))
    case Stmt.For(_, _, _, _, _) => Result.Good(false :: t.tail)
    case Stmt.While(_, _) => Result.Good(false :: t.tail)
    case _: Stmt => Result.Good(false :: t)
    case Block(_, stmts) => Result.Good(t.take(stmts.length).foldLeft(false)(_ || _) :: t.drop(stmts.length))
    case MethodDecl(IrVoidableType(typ), _, _, _) => typ match {
      case VoidableType.VoidT => Result.Good(t.tail)
      case _ =>
        if (t.head) Result.Good(t.tail)
        else Result.Error(SemanticError(node.getSource, "All non-void methods must return a result"), t.tail)
    }
    case _ => Result.Good(t)
  }

}


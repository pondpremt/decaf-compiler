package semanticchecking

import ir._
import symboltable.{Descriptor, SymbolTable}

object IDChecker extends SemanticChecker[Unit] {

  // Checked: 2, 3, 10, 11.a, [12]

  type T = Unit

  def initCheckerState: T = Unit

  def checkDeclared(st: SymbolTable, t: T, id: ID): Result[T] = st.lookup(id.name) match {
    case Some(_) => Result.Good(t)
    case _ => Result.Error(SemanticError(id.getSource, "Identifier " + id.name + " is used before it is declared"), t)
  }

  def enter(node: Ir, st: SymbolTable, t: T): Result[T] = node match {
    case Location.Var(id) => checkDeclared(st, t, id)
    case Expr.Length(id) => checkDeclared(st, t, id)
    case MethodCall(id, _) => checkDeclared(st, t, id)
    case Stmt.For(id, _, _, _, _) => st.lookup(id.name) match {
      case Some(Descriptor.Variable(_, PrimitiveType.IntT)) => Result.Good(t)
      case _ => Result.Error(SemanticError(id.getSource, "For loop index must have been declared an integer variable"), t)
    }
    case Location.Cell(id, _) => st.lookup(id.name) match {
      case Some(Descriptor.Array(_, _, _)) => Result.Good(t)
      case Some(_) => Result.Error(SemanticError(id.getSource, "For all locations of the form id[expr], id must be an array variable"), t)
      case _ => Result.Error(SemanticError(id.getSource, "Identifier " + id.name + " is used before it is declared"), t)
    }
    case _ => Result.Good(t)
  }

  def leave(node: Ir, st: SymbolTable, t: T): Result[T] = node match {
    case _: Program => st.lookup("main") match {
      case Some(Descriptor.Method(_, FunctionType(Nil, VoidableType.VoidT))) => Result.Good(t)
      case _ => Result.Error(SemanticError(node.getSource, "The program must contain a definition for void main () that has no parameters"), t)
    }
    case _ => Result.Good(t)
  }

}


package semanticchecking

import ir.Ir
import symboltable.{STReadListener, SymbolTable}

abstract class SemanticChecker[T] extends STReadListener[(List[SemanticError], T)] {

  final override def initState: (List[SemanticError], T) = (Nil, initCheckerState)

  def initCheckerState: T

  def enter(node: Ir, st: SymbolTable, t: (List[SemanticError], T)): (List[SemanticError], T) =
    enter(node, st, t._2) match {
      case Result.Good(_t) => (t._1, _t)
      case Result.Error(e, _t) => (e :: t._1, _t)
    }

  def leave(node: Ir, st: SymbolTable, t: (List[SemanticError], T)): (List[SemanticError], T) =
    leave(node, st, t._2) match {
      case Result.Good(_t) => (t._1, _t)
      case Result.Error(e, _t) => (e :: t._1, _t)
    }

  def enter(node: Ir, st: SymbolTable, t: T): Result[T]

  def leave(node: Ir, st: SymbolTable, t: T): Result[T]

  sealed abstract class Result[A]

  object Result {

    final case class Good[A](t: A) extends Result[A]

    final case class Error[A](e: SemanticError, t: A) extends Result[A]

  }

}




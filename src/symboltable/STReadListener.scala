package symboltable

import ir.Ir

abstract class STReadListener[T] extends STListener[T] {

  final override def enter(node: Ir, s: S): S = (s._1, enter(node, s._1, s._2))

  final override def leave(node: Ir, s: S): S = (s._1, leave(node, s._1, s._2))

  def enter(node: Ir, st: SymbolTable, t: T): T

  def leave(node: Ir, st: SymbolTable, t: T): T
}



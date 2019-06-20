package ir

import util.State

abstract class STListener[S] {

  def init: S

  def enter(node: Ir, st: SymbolTable): State[S, SymbolTable] = State.pure(SymbolTable.base)

  def leave(node: Ir, st: SymbolTable): State[S, SymbolTable] = State.pure(SymbolTable.base)

}

object STListener1 extends STListener[Unit] {

  type S = Unit

  def init: S = ()

}


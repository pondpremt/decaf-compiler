package ir

import util.State

abstract class STDependentListener[S, A] {

  def init: S

  def enter(node: Ir, st: SymbolTable): State[S, Unit] = State.pure(())

  def leave(node: Ir, st: SymbolTable, vals: List[Result[A]]): State[S, A]

}

abstract class STDependentListenerList[S, A] extends STDependentListener[S, A]

case object STDependentListenerNil extends STDependentListenerList[Unit, Unit] {

  type S = Unit
  type A = Unit

  def init: S = ()

  def leave(node: Ir, st: SymbolTable, vals: List[Result[A]]): State[S, A] = State.pure(())
}

case class STDependentListenerCons[S1, A1, S2, A2](x: STDependentListener[S1, A1], xs: STDependentListenerList[S2, A2])
  extends STDependentListenerList[(S1, S2), (A1, A2)] {

  type S = (S1, S2)
  type A = (A1, A2)

  def init: S = (x.init, xs.init)

  override def enter(node: Ir, st: SymbolTable): State[S, Unit] = State { s: S => {
    val (_, _s1) = x.enter(node, st)(s._1)
    val (_, _s2) = xs.enter(node, st)(s._2)
    ((), (_s1, _s2))
  }
  }

  def leave(node: Ir, st: SymbolTable, vals: List[Result[A]]): State[S, A] = State { s: S => {
    val (vals1, vals2) = vals.unzip
    val (val1, _s1) = x.leave(node, st, vals1)(s._1)
    val (val2, _s2) = xs.leave(node, st, vals2)(s._2)
    ((val1, val2), (_s1, _s2))
  }
  }

}


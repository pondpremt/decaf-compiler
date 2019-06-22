package ir

abstract class STListener[T] extends Listener[(SymbolTable, T)] {

  override type S = (SymbolTable, T)

  final def init: S = (SymbolTable.base, initState)

  def initState: T

}

abstract class STReadListener[T] extends STListener[T] {

  final override def enter(node: Ir, s: S): S = (s._1, enter(node, s._1, s._2))

  final override def leave(node: Ir, s: S): S = (s._1, leave(node, s._1, s._2))

  def enter(node: Ir, st: SymbolTable, t: T): T

  def leave(node: Ir, st: SymbolTable, t: T): T
}

case class STListenerPair[T1, T2](l1: STListener[T1], l2: STListener[T2]) extends STListener[(T1, T2)] {

  type T = (T1, T2)

  def initState: T = (l1.initState, l2.initState)

  override def enter(node: Ir, s: S): S = {
    val (st, (s1, s2)) = s
    val (_st, _s1) = l1.enter(node, (st, s1))
    val (__st, _s2) = l2.enter(node, (_st, s2))
    (__st, (_s1, _s2))
  }

  override def leave(node: Ir, s: S): S = {
    val (st, (s1, s2)) = s
    val (_st, _s1) = l1.leave(node, (st, s1))
    val (__st, _s2) = l2.leave(node, (_st, s2))
    (__st, (_s1, _s2))
  }

}

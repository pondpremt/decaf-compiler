package symboltable

import ir.Ir

final case class STListenerPair[T1, T2](l1: STListener[T1], l2: STListener[T2]) extends STListener[(T1, T2)] {

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


package ir

sealed abstract class Result[A]

import util.State

object Result {

  final case class OfNode[A](a: A) extends Result[A]

  final case class OfOption[A](a: Option[A]) extends Result[A]

  final case class OfList[A](a: List[A]) extends Result[A]

  implicit def asPair[A1, A2]: Result[(A1, A2)] => (Result[A1], Result[A2]) = {
    case OfNode((a1, a2)) => (OfNode(a1), OfNode(a2))
    case OfList(as) => {
      val (as1, as2) = as.unzip
      (OfList(as1), OfList(as2))
    }
    case OfOption(Some((a1, a2))) => (OfOption(Some(a1)), OfOption(Some(a2)))
    case OfOption(None) => (OfOption(None), OfOption(None))
  }

}

abstract class Listener[S, A] {

  def init: S

  def enter(node: Ir, st: SymbolTable): State[S, Unit] = State.pure(())

  def leave(node: Ir, st: SymbolTable, vals: List[Result[A]]): State[S, A]

}

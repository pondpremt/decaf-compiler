package ir

abstract class Listener[T] {

  type S = T

  def init: S

  def enter(node: Ir, s: S): S

  def leave(node: Ir, s: S): S

}

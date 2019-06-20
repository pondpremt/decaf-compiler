package ir

import util.State

case class WithSTListener[S1, S2, A](stl: STListener[S1], sdl: STDependentListener[S2, A])
  extends Listener[(SymbolTable, S1, S2), A] {

  type S = (SymbolTable, S1, S2)

  def init: S = (SymbolTable.base, stl.init, sdl.init)

  def enter(node: Ir): State[S, Unit] = State { s: S => {
    val (st, stlState) = stl.enter(node, s._1)(s._2)
    val (_, sdlState) = sdl.enter(node, st)(s._3)
    ((), (st, stlState, sdlState))
  }
  }

  def leave(node: Ir, vals: List[Result[A]]): State[S, A] = State { s: S => {
    val (st, stlState) = stl.leave(node, s._1)(s._2)
    val (res, sdlState) = sdl.leave(node, st, vals)(s._3)
    (res, (st, stlState, sdlState))
  }
  }

}





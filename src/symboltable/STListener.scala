package symboltable

import ir.Listener

abstract class STListener[T] extends Listener[(SymbolTable, T)] {

  override type S = (SymbolTable, T)

  final def init: S = (SymbolTable.base(STScope.Global), initState)

  def initState: T

}


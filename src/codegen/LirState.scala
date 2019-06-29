package codegen

import util.State.{get, pure, put}

object LirState {

  final case class Fragment(code: List[lir.Stmt], ret: Option[lir.Location.Name])

  final case class Builder(stack: List[Fragment], decls: List[lir.Local], program: lir.Program, counter: Int, isGlobal: Boolean)

  type State[A] = util.State[Builder, A]

  // --------------------------------------
  // State management classes and functions
  // --------------------------------------

  def top: State[Fragment] = get >>| (s => s.stack.head)

  def wordSize: Long = 8L

  def push(fragment: Fragment): State[Builder] = get >>= (s => put(s.copy(stack = fragment :: s.stack)))

  def pop: LirState.State[Fragment] = get >>= (s =>
    put(s.copy(stack = s.stack.tail)) >> pure(s.stack.head))

  def append(stmt: lir.Stmt): State[Builder] = extend(List(stmt))

  def append(stmt: lir.Stmt, ret: Option[lir.Location.Name]): State[Builder] = extend(List(stmt), ret)

  def extend(stmts: List[lir.Stmt]): State[Builder] = top.flatMap(f => extend(stmts, f.ret))

  def extend(stmts: List[lir.Stmt], ret: Option[lir.Location.Name]): State[Builder] = get >>= (s => {
    val f1 = s.stack.head
    val f2 = f1.copy(code = stmts ::: f1.code, ret = ret)
    put(s.copy(stack = f2 :: s.stack.tail))
  })

  def declVar(name: String): State[Builder] = get >>= (s => {
    if (s.isGlobal)
      put(s.copy(program = s.program.copy(decls = lir.Global(name, wordSize) :: s.program.decls)))
    else
      put(s.copy(decls = lir.Local(name, wordSize) :: s.decls))
  })

  def declArr(name: String, size: Long): State[Builder] = get >>= (s => {
    if (s.isGlobal)
      put(s.copy(program = s.program.copy(decls = lir.Global(name, size * wordSize) :: s.program.decls)))
    else
      put(s.copy(decls = lir.Local(name, size * wordSize) :: s.decls))
  })

  def toggleIsGlobal: State[Builder] = get >>= (s => put(s.copy(isGlobal = !s.isGlobal)))

  def nextCount: State[Int] = get >>= (s => put(s.copy(counter = s.counter + 1)) >> pure(s.counter))

  def nextTmp: State[lir.Location.Name] = nextCount >>| (counter => lir.Location.Name("__&tmp_" + counter))

}

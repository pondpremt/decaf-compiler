package codegen

import util.State.{get, pure, put}

object State {

  final case class Fragment(code: List[lir.Stmt], ret: lir.Location.Name)

  final case class Builder(stack: List[Fragment], decls: List[lir.Local], program: lir.Program, counter: Int, isGlobal: Boolean)

  type BState[A] = util.State[Builder, A]

  // --------------------------------------
  // State management classes and functions
  // --------------------------------------

  def top: BState[Fragment] = get >>| (s => s.stack.head)

  def wordSize: Long = 8L

  def push(fragment: Fragment): BState[Unit] = get >>= (s => put(s.copy(stack = fragment :: s.stack)))

  def pop: State.BState[Fragment] = get >>= (s =>
    put(s.copy(stack = s.stack.tail)) >> pure(s.stack.head))

  def append(stmt: lir.Stmt): BState[Unit] = extend(List(stmt))

  def append(stmt: lir.Stmt, ret: lir.Location.Name): BState[Unit] = extend(List(stmt), ret)

  def extend(stmts: List[lir.Stmt]): BState[Unit] = top.flatMap(f => extend(stmts, f.ret))

  def extend(stmts: List[lir.Stmt], ret: lir.Location.Name): BState[Unit] = get >>= (s => {
    val f1 = s.stack.head
    val f2 = f1.copy(code = stmts ::: f1.code, ret = ret)
    put(s.copy(stack = f2 :: s.stack.tail))
  })

  def declVar(name: String): BState[Unit] = get >>= (s => {
    if (s.isGlobal)
      put(s.copy(program = s.program.copy(decls = lir.Global(name, wordSize) :: s.program.decls)))
    else
      put(s.copy(decls = lir.Local(name, wordSize) :: s.decls))
  })

  def declArr(name: String, size: Long): BState[Unit] = get >>= (s => {
    if (s.isGlobal)
      put(s.copy(program = s.program.copy(decls = lir.Global(name, size * wordSize) :: s.program.decls)))
    else
      put(s.copy(decls = lir.Local(name, size * wordSize) :: s.decls))
  })

  def toggleIsGlobal: BState[Unit] = get >>= (s => put(s.copy(isGlobal = !s.isGlobal)))

  def nextCount: BState[Int] = get >>= (s => put(s.copy(counter = s.counter + 1)) >> pure(s.counter))

  def nextTmp: BState[lir.Location.Name] = nextCount >>| (counter => lir.Location.Name("__&tmp_" + counter))

}

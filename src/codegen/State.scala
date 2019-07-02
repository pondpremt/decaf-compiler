package codegen

import lir._
import util.State.{get, pure, put}

object State {

  final case class Builder(code: List[Stmt], decls: List[Local], program: Program, counter: Int, isGlobal: Boolean)

  type BState[A] = util.State[Builder, A]

  // --------------------------------------
  // State management classes and functions
  // --------------------------------------

  def wordSize: Long = 8L

  def append(stmt: Stmt): BState[Unit] = extend(List(stmt))

  def extend(stmts: List[Stmt]): BState[Unit] =
    get >>= (s => put(s.copy(code = stmts ::: s.code)))

  def declVar(name: String): BState[Unit] = get >>= (s => {
    if (s.isGlobal)
      put(s.copy(program = s.program.copy(decls = Global(name, wordSize) :: s.program.decls)))
    else
      put(s.copy(decls = Local(name, wordSize) :: s.decls))
  })

  def declArr(name: String, size: Long): BState[Unit] = get >>= (s => {
    if (s.isGlobal)
      put(s.copy(program = s.program.copy(decls = Global(name, size * wordSize) :: s.program.decls)))
    else
      put(s.copy(decls = Local(name, size * wordSize) :: s.decls))
  })

  /** Declare the string 'text' and return the name */
  def declString(text: String): BState[String] = for {
    i <- nextCount
    name = ".STR_" + i
    str = Str(name, text)
    s <- get
    _ <- put(s.copy(program = s.program.copy(strings = str :: s.program.strings)))
  } yield name

  def toggleIsGlobal: BState[Unit] = get >>= (s => put(s.copy(isGlobal = !s.isGlobal)))

  def nextCount: BState[Int] = get >>= (s => put(s.copy(counter = s.counter + 1)) >> pure(s.counter))

  def nextTmp: BState[Location.Name] = for {
    name <- nextCount >>| ("TMP_" + _)
    _ <- declVar(name)
  } yield Location.Name(name)
}

package codegen

import lir._
import util.State.{get, pure, put}

object LirGeneratorState {

  final case class Builder(code: List[Stmt],
                           decls: List[Local],
                           program: Program,
                           counter: Int,
                           isGlobal: Boolean,
                           break: String, // Branch to jump to when encounter break
                           continue: String // Branch to jump to when encounter continue
                          )

  type BState[A] = util.State[Builder, A]

  // --------------------------------------
  // State management classes and functions
  // --------------------------------------

  def append(stmt: Stmt): BState[Unit] = extend(List(stmt))

  def extend(stmts: List[Stmt]): BState[Unit] =
    get >>= (s => put(s.copy(code = stmts.foldLeft(s.code)((code, stmt) => stmt :: code))))

  def declVar(name: String): BState[Unit] = get >>= (s => {
    if (s.isGlobal)
      put(s.copy(program = s.program.copy(decls = Global(name, Lir.wordSize) :: s.program.decls)))
    else
      put(s.copy(decls = Local(name, Lir.wordSize) :: s.decls))
  })

  def declArr(name: String, size: Long): BState[Unit] = get >>= (s => {
    if (s.isGlobal)
      put(s.copy(program = s.program.copy(decls = Global(name, size * Lir.wordSize) :: s.program.decls)))
    else
      put(s.copy(decls = Local(name, size * Lir.wordSize) :: s.decls))
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

  def getCount: BState[Int] = get >>| (_.counter)

  def nextTmp: BState[Location.Name] = for {
    name <- nextCount >>| ("TMP_" + _)
    _ <- declVar(name)
  } yield Location.Name(name)
}

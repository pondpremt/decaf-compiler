package lir

sealed abstract class Lir

final case class Program(decls: List[Global], strings: List[Str], methods: List[Method]) extends Lir

final case class Method(name: String, decls: List[Local], stmts: List[Stmt]) extends Lir

final case class Local(name: String, size: Long) extends Lir

final case class Global(name: String, size: Long) extends Lir

final case class Str(name: String, text: String) extends Lir

sealed abstract class Stmt

final case class Label(name: String) extends Stmt

sealed abstract class Copy extends Stmt

object Copy {

  final case class Mov(src: Source, dest: Location) extends Copy

  final case class Cmove(src: Register, dest: Register) extends Copy

  final case class Cmovne(src: Register, dest: Register) extends Copy

  final case class Cmovg(src: Register, dest: Register) extends Copy

  final case class Cmovl(src: Register, dest: Register) extends Copy

  final case class Cmovge(src: Register, dest: Register) extends Copy

  final case class Cmovle(src: Register, dest: Register) extends Copy

}

sealed abstract class Stack extends Stmt

object Stack {

  // final case class Enter(size: Long) extends Stack

  final case object Leave extends Stack

  final case class Push(src: Source) extends Stack

  final case class Pop(dest: Location) extends Stack

}

sealed abstract class Control extends Stmt

object Control {

  final case class Call(target: Target) extends Control

  final case object Ret extends Control

  final case class Jmp(target: Target) extends Control

  final case class Je(target: Target) extends Control

  final case class Jne(target: Target) extends Control

  final case class Syscall() extends Control

}

sealed abstract class Arith extends Stmt

object Arith {

  final case class Add(src: Source, dest: Location) extends Arith

  final case class Sub(src: Source, dest: Location) extends Arith

  final case class Imul(src: Source, dest: Register) extends Arith

  final case class Idiv(divisor: Source) extends Arith

  final case class Shr(reg: Register) extends Arith

  final case class Shl(reg: Register) extends Arith

  final case class Ror(src: Source, dest: Location) extends Arith

  final case class Cmp(src: Source, dest: Location) extends Arith

}


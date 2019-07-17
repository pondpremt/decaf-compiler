package lir

sealed abstract class Lir

object Lir {

  val wordSize: Long = 8

}

final case class Program(decls: List[Global], strings: List[Str], methods: List[Method]) extends Lir

final case class Method(name: String, decls: List[Local], stmts: List[Stmt]) extends Lir

final case class Local(name: String, size: Long) extends Lir

final case class Global(name: String, size: Long) extends Lir

final case class Str(name: String, text: String) extends Lir

sealed abstract class Stmt

sealed trait TwoOp extends Stmt {
  val src: Source
  val dst: Location
}

sealed abstract class Copy extends Stmt with TwoOp

object Copy {

  final case class Lea(src: Source, dst: Location) extends Copy

  final case class Mov(src: Source, dst: Location) extends Copy

  // Need to convert both src and dst to bare registers
  final case class Cmov(op: CmpOp, src: Source, dst: Location) extends Copy

}

sealed abstract class Stack extends Stmt

object Stack {

  final case class Enter(size: Long) extends Stack

  final case object Leave extends Stack

  final case class Push(src: Source) extends Stack

  final case class Pop(dst: Location) extends Stack

}

sealed abstract class Control extends Stmt

object Control {

  final case class Call(target: String) extends Control

  final case object Ret extends Control

  final case class Jmp(target: String) extends Control

  final case class Cjmp(op: CmpOp, target: String) extends Control

  final case object Syscall extends Control

  final case class Label(name: String) extends Control

  final case object Nop extends Control

}

// TODO simplify LirGenerator to use Arith3

sealed abstract class Arith3 extends Stmt {
  val arg1: Source
  val arg2: Source
  val dst: Location
}

object Arith3 {

  final case class Add(arg1: Source, arg2: Source, dst: Location) extends Arith3

  final case class Sub(arg1: Source, arg2: Source, dst: Location) extends Arith3

  final case class Mul(arg1: Source, arg2: Source, dst: Location) extends Arith3

  final case class Div(arg1: Source, arg2: Source, dst: Location) extends Arith3

  final case class Mod(arg1: Source, arg2: Source, dst: Location) extends Arith3

  final case class Cmp(op: CmpOp, arg1: Source, arg2: Source, dst: Location) extends Arith3

}

sealed abstract class Arith extends Stmt

object Arith {

  final case class Add(src: Source, dst: Location) extends Arith with TwoOp

  final case class Sub(src: Source, dst: Location) extends Arith with TwoOp

  final case class Imul(src: Source, dst: Register) extends Arith

  final case class Idiv(divisor: Source) extends Arith

  final case class Shr(reg: Register) extends Arith

  final case class Shl(reg: Register) extends Arith

  final case class Ror(src: Source, dst: Location) extends Arith with TwoOp

  final case class Cmp(src: Source, dst: Location) extends Arith with TwoOp

}


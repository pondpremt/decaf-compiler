package codegen

import lir._

object CodeGenerator {

  def indent: String = "\t"

  def gen(n: Program): String =
    n.decls.map(gen).mkString + n.strings.map(gen).mkString + n.methods.map(gen).mkString

  def gen(n: Global): String = s".comm\t${n.name}, ${n.size}\n"

  def gen(n: Str): String = s"${n.name}:\n" + indent + s".string\t${n.text}\n"

  def gen(n: Method): String = {
    val global = if (n.name == "main") indent + ".globl\tmain\n" else ""
    global + s"${n.name}:\n" + n.decls.map(gen).mkString + n.stmts.map(gen).mkString
  }

  def gen(n: Local): String = indent + s"<LIR> LOCAL\t${n.name} ${n.size}\n"

  def gen(_n: Stmt): String = _n match {
    case n: Copy => gen(n)
    case n: Stack => gen(n)
    case n: Control => gen(n)
    case n: Arith => gen(n)
  }

  def gen(n: Copy): String = n match {
    case Copy.Lea(src, dst) => indent + s"leaq\t${gen(src)}, ${gen(dst)}\n"
    case Copy.Mov(src, dst) => indent + s"movq\t${gen(src)}, ${gen(dst)}\n"
    case Copy.Cmove(src, dst) => indent + s"cmove\t${gen(src)}, ${gen(dst)}\n"
    case Copy.Cmovne(src, dst) => indent + s"cmovne\t${gen(src)}, ${gen(dst)}\n"
    case Copy.Cmovl(src, dst) => indent + s"cmovl\t${gen(src)}, ${gen(dst)}\n"
    case Copy.Cmovle(src, dst) => indent + s"cmovle\t${gen(src)}, ${gen(dst)}\n"
    case Copy.Cmovg(src, dst) => indent + s"cmovg\t${gen(src)}, ${gen(dst)}\n"
    case Copy.Cmovge(src, dst) => indent + s"cmovge\t${gen(src)}, ${gen(dst)}\n"
  }

  def gen(n: Stack): String = n match {
    case Stack.Push(src) => indent + s"pushq\t${gen(src)}\n"
    case Stack.Pop(dst) => indent + s"popq\t${gen(dst)}\n"
    case Stack.Enter(size) => indent + s"enter\t$$$size, $$0\n"
    case Stack.Leave => indent + "leave\n"
  }

  def gen(n: Control): String = n match {
    case Control.Call(target) => indent + s"call\t$target\n"
    case Control.Ret => indent + "ret\n"
    case Control.Jmp(target) => indent + s"jmp\t$target\n"
    case Control.Je(target) => indent + s"je\t$target\n"
    case Control.Jne(target) => indent + s"jne\t$target\n"
    case Control.Syscall => indent + "syscall\n"
    case Control.Label(name) => s"$name:\n"
    case Control.Nop => ""
  }

  def gen(n: Arith): String = n match {
    case Arith.Add(src, dst) => indent + s"addq\t${gen(src)}, ${gen(dst)}\n"
    case Arith.Sub(src, dst) => indent + s"subq\t${gen(src)}, ${gen(dst)}\n"
    case Arith.Imul(src, dst) => indent + s"imulq\t${gen(src)}, ${gen(dst)}\n"
    case Arith.Idiv(divisor) => indent + s"idivq\t${gen(divisor)}\n"
    case Arith.Shr(reg) => indent + s"shrq\t${gen(reg)}\n"
    case Arith.Shl(reg) => indent + s"shlq\t${gen(reg)}\n"
    case Arith.Ror(src, dst) => indent + s"rorq\t${gen(src)}, ${gen(dst)}\n"
    case Arith.Cmp(src, dst) => indent + s"cmpq\t${gen(src)}, ${gen(dst)}\n"
  }

  def gen(n: Source): String = n match {
    case Source.Lit(value) => "$" + value
    case Source.Loc(loc) => gen(loc)
  }

  def gen(n: Location): String = n match {
    case Location.Name(name) => name
    case Location.Reg(reg) => gen(reg)
    case Location.Addr(offset, reg1, reg2, scale) =>
      val s1 = offset match {
        case Left(0L) => ""
        case Left(value) => value
        case Right(name) => name
      }
      val s2 = s1 + "(" + (reg1 match {
        case Some(r) => gen(r);
        case _ => ""
      })
      val s3 = s2 + ((reg2, scale) match {
        case (Some(r), 1L) => ", " + gen(r) + ")"
        case (Some(r), value) => ", " + gen(r) + s", $value)"
        case _ => ")"
      })
      s3
  }

  def gen(n: Register): String = s"%${n.name}"

}

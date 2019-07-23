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

  def gen(n: Local): String = indent + s"!LOCAL\t${n.name} ${n.size}\n"

  def gen(_n: Stmt): String = _n match {
    case n: Copy => gen(n)
    case n: Stack => gen(n)
    case n: Control => gen(n)
    case n: Arith => gen(n)
    case n: Arith3 => gen(n)
  }

  def gen(n: Copy): String = n match {
    case Copy.Lea(src, dst) => indent + s"leaq\t${gen(src)}, ${gen(dst)}\n"
    case Copy.Mov(src, dst) => indent + s"movq\t${gen(src)}, ${gen(dst)}\n"
    case Copy.Cmov(op, src, dst) => indent + s"cmov${gen(op)}\t${gen(src)}, ${gen(dst)}\n"
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
    case Control.Cjmp(op, target) => indent + s"j${gen(op)}\t$target\n"
    case Control.Throw(target) => indent + s"jmp\t$target\n"
    case Control.Syscall => indent + "syscall\n"
    case Control.Label(name) => s"$name:\n"
    case Control.Nop => ""
  }

  def gen(n: Arith3): String = n match {
    case Arith3.Add(arg1, arg2, dst) => indent + s"!arith3\t${gen(dst)} = ${gen(arg1)} + ${gen(arg2)}\n"
    case Arith3.Sub(arg1, arg2, dst) => indent + s"!arith3\t${gen(dst)} = ${gen(arg1)} - ${gen(arg2)}\n"
    case Arith3.Mul(arg1, arg2, dst) => indent + s"!arith3\t${gen(dst)} = ${gen(arg1)} * ${gen(arg2)}\n"
    case Arith3.Div(arg1, arg2, dst) => indent + s"!arith3\t${gen(dst)} = ${gen(arg1)} / ${gen(arg2)}\n"
    case Arith3.Mod(arg1, arg2, dst) => indent + s"!arith3\t${gen(dst)} = ${gen(arg1)} % ${gen(arg2)}\n"
    case Arith3.Cmp(op, arg1, arg2, dst) => indent + s"!arith3\t${gen(dst)} = ${gen(arg1)} ${gen(op)} ${gen(arg2)}\n"
  }

  def gen(n: Arith): String = n match {
    case Arith.Add(src, dst) => indent + s"addq\t${gen(src)}, ${gen(dst)}\n"
    case Arith.Sub(src, dst) => indent + s"subq\t${gen(src)}, ${gen(dst)}\n"
    case Arith.Imul(src, dst) => indent + s"imulq\t${gen(src)}, ${gen(dst)}\n"
    case Arith.Idiv(divisor) => indent + "cqto\n" + indent + s"idivq\t${gen(divisor)}\n"
    case Arith.Shr(reg) => indent + s"shrq\t${gen(reg)}\n"
    case Arith.Shl(reg) => indent + s"shlq\t${gen(reg)}\n"
    case Arith.Ror(src, dst) => indent + s"rorq\t${gen(src)}, ${gen(dst)}\n"
    case Arith.Cmp(src, dst) => indent + s"cmpq\t${gen(src)}, ${gen(dst)}\n"
  }

  def gen(n: Source): String = n match {
    case Source.Lit(value) => "$" + value
    case Source.Str(name) => "!`" + name
    case Source.Loc(loc) => gen(loc)
  }

  def gen(n: Location): String = n match {
    case Location.Name(name) => name
    case Location.Array(base, Left(offset)) => s"$base[${gen(offset)}]"
    case Location.Array(base, Right(offset)) => s"$base[${gen(offset)}]"
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
      s3.replaceAll("\\(\\)", "")
  }

  def gen(n: Register): String = s"%${n.name}"

  def gen(op: CmpOp): String = op match {
    case CmpOp.E => "e"
    case CmpOp.Ne => "ne"
    case CmpOp.L => "l"
    case CmpOp.G => "g"
    case CmpOp.Le => "le"
    case CmpOp.Ge => "ge"
  }

}

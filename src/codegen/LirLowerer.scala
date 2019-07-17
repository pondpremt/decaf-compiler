package codegen

import lir.Conversion._
import lir.Registers._
import lir._

/** Lower lir to be closer to assembly by making sure that
  * (1) Two-operand instructions have at most 1 address operands.
  * (2) Cmovx instructions take registers as operands
  * (3) Arith3 instructions are translated to lower-level instructions
  * (4) String moves are translated to lea intrusctions
  *
  * Note that we don't resolve names and arrays, and treat them as address operands
  */
object LirLowerer {

  def lower(n: Program): Program = n.copy(methods = n.methods.map(lower))

  def lower(n: Method): Method = n.copy(stmts = n.stmts.flatMap(lower))

  def lower(_n: Stmt): List[Stmt] = _n match {
    case n: Copy => lower(n)
    case n: Arith => lower(n)
    case n: Arith3 => lower(n)
    case _ => List(_n)
  }

  def lower(_n: Copy): List[Stmt] = _n match {
    case Copy.Mov(Source.Str(name), dst) => lower(Copy.Lea(Location.Addr(Right(name), Some(rip), None, 1L), dst))
    case n: Copy.Mov => lowerCopy(n)
    case n: Copy.Lea => lowerCopy(n)
    case n: Copy.Cmov => lowerCmov(n)
  }

  def lowerCopy(n: Copy): List[Stmt] = (n.src, n.dst) match {
    case (Source.Lit(_), _) | (Source.Loc(Location.Reg(_)), _) | (_, Location.Reg(_)) => List(n)
    case _ => n match {
      case _: Copy.Mov => List(Copy.Mov(n.src, r10), Copy.Mov(r10, n.dst))
      case _: Copy.Lea => List(Copy.Lea(n.src, r10), Copy.Mov(r10, n.dst))
      case _ => throw new RuntimeException
    }
  }

  def lowerCmov(n: Copy.Cmov): List[Stmt] = (n.src, n.dst) match {
    case (Source.Loc(Location.Reg(_)), Location.Reg(_)) => List(n)
    case (_, Location.Reg(`r10`)) =>
      List(Copy.Mov(n.src, r11), Copy.Cmov(n.op, r11, r10))
    case (_, Location.Reg(reg)) =>
      List(Copy.Mov(n.src, r10), Copy.Cmov(n.op, r10, reg))
    case (Source.Loc(Location.Reg(`r10`)), _) =>
      List(Copy.Mov(n.dst, r11), Copy.Cmov(n.op, r10, r11), Copy.Mov(r11, n.dst))
    case (Source.Loc(Location.Reg(reg)), _) =>
      List(Copy.Mov(n.dst, r10), Copy.Cmov(n.op, reg, r10), Copy.Mov(r10, n.dst))
    case _ =>
      List(Copy.Mov(n.src, r10), Copy.Mov(n.dst, r11), Copy.Cmov(n.op, r10, r11), Copy.Mov(r11, n.dst))
  }

  def lower(_n: Arith): List[Stmt] = _n match {
    case n: Arith.Add => lowerArith(n, Arith.Add)
    case n: Arith.Sub => lowerArith(n, Arith.Sub)
    case n: Arith.Ror => lowerArith(n, Arith.Ror)
    case n: Arith.Cmp => lowerArith(n, Arith.Cmp)
    case _ => List(_n)
  }

  def lowerArith(n: TwoOp, instr: (Source, Location) => Stmt): List[Stmt] = (n.src, n.dst) match {
    case (Source.Lit(_), _) | (Source.Loc(Location.Reg(_)), _) | (_, Location.Reg(_)) => List(n)
    case _ => List(Copy.Mov(n.src, r10), instr(r10, n.dst))
  }

  def lower(_n: Arith3): List[Stmt] = _n match {
    case Arith3.Add(arg1, arg2, dst) => lower(Copy.Mov(arg1, dst)) ++ lower(Arith.Add(arg2, dst))
    case Arith3.Sub(arg1, arg2, dst) => lower(Copy.Mov(arg1, dst)) ++ lower(Arith.Sub(arg2, dst))
    case Arith3.Mul(arg1, arg2, Location.Reg(reg)) => List(Copy.Mov(arg1, reg), Arith.Imul(arg2, reg))
    case Arith3.Mul(arg1, arg2, dst) => List(Copy.Mov(arg1, r10), Arith.Imul(arg2, r10), Copy.Mov(r10, dst))
    case Arith3.Div(arg1, arg2, dst) => List(Copy.Mov(arg1, rax), Arith.Idiv(arg2), Copy.Mov(rax, dst))
    case Arith3.Mod(arg1, arg2, dst) => List(Copy.Mov(arg1, rax), Arith.Idiv(arg2), Copy.Mov(rdx, dst))
    case Arith3.Cmp(op, Source.Loc(loc), arg2, dst) => lower(Arith.Cmp(arg2, loc)) ++ List(Copy.Mov(0L, dst)) ++
      lower(Copy.Cmov(op, 1L, dst))
    case Arith3.Cmp(op, arg1, arg2, dst) => List(Copy.Mov(arg1, r10)) ++ lower(Arith.Cmp(arg2, r10)) ++
      List(Copy.Mov(0L, dst)) ++ lower(Copy.Cmov(op, 1L, dst))
  }

}


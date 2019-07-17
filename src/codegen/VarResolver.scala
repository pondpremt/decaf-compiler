package codegen

import lir.Conversion._
import lir.Registers._
import lir._
import util.State.{pure, update}


private object VarResolverState {

  type StateM[A] = util.State[MyState, A]

  def init = MyState(Nil)

  def append(stmt: Stmt): StateM[Unit] = update(s => s.copy(code = stmt :: s.code))

  case class MyState(code: List[Stmt])

}

/** Take an LIR and convert names to appropriate addressing.
  * Input instructions may not have more than 1 array operand */
object VarResolver {

  import VarResolverState._

  def run(n: Program): Program = n.copy(methods = n.methods.map(resolve))

  private def initMem(size: Long): List[Stmt] = for {
    i <- (-size / Lir.wordSize until 0).toList
  } yield Copy.Mov(0L, Location.Addr(Left(i * Lir.wordSize), Some(rbp), None, 1L))

  private def resolve: Method => Method = {
    case Method(name, decls, stmts) =>
      val mapInit: Map[String, Long] = Map()
      val (map, size) = decls.foldLeft((mapInit, 0L)) { (accum: (Map[String, Long], Long), decl: Local) =>
        val newBase = accum._2 + decl.size
        (accum._1 + ((decl.name, newBase)), newBase)
      }
      Method(
        name, Nil, Stack.Enter(size) :: initMem(size) ::: stmts.flatMap(s => resolve(s, map)(init)._2.code.reverse))
  }

  private def resolve(_n: Stmt, m: Map[String, Long]): StateM[Unit] = _n match {
    case n: Copy => resolve(n, m)
    case n: Stack => resolve(n, m)
    case n: Arith => resolve(n, m)
    case n: Arith3 => resolve(n, m) // in theory, we should have lowered arith3 to arith; here for completeness
    case n: Control => append(n)
  }

  private def resolve(n: Copy, m: Map[String, Long]): StateM[Unit] = for {
    src2 <- resolve(n.src, m)
    dst2 <- resolve(n.dst, m)
    _ <- n match {
      case Copy.Lea(_, _) => append(Copy.Lea(src2, dst2))
      case Copy.Mov(_, _) => append(Copy.Mov(src2, dst2))
      case Copy.Cmov(op, _, _) => append(Copy.Cmov(op, src2, dst2))
    }
  } yield ()

  private def resolve(n: Stack, m: Map[String, Long]): StateM[Unit] = n match {
    case Stack.Push(src) => resolve(src, m) >>= (src2 => append(Stack.Push(src2)))
    case Stack.Pop(dst) => resolve(dst, m) >>= (dst2 => append(Stack.Pop(dst2)))
    case _ => append(n)
  }

  private def resolve(_n: Arith, m: Map[String, Long]): StateM[Unit] = _n match {
    case Arith.Imul(src, dst) => resolve(src, m) >>= (src2 => append(Arith.Imul(src2, dst)))
    case Arith.Idiv(divisor) => resolve(divisor, m) >>= (divisor2 => append(Arith.Idiv(divisor2)))
    case n: TwoOp => for {
      src2 <- resolve(n.src, m)
      dst2 <- resolve(n.dst, m)
      _ <- append(n match {
        case _: Arith.Add => Arith.Add(src2, dst2)
        case _: Arith.Sub => Arith.Sub(src2, dst2)
        case _: Arith.Ror => Arith.Ror(src2, dst2)
        case _: Arith.Cmp => Arith.Cmp(src2, dst2)
        case _ => throw new RuntimeException
      })
    } yield ()
    case _ => throw new RuntimeException
  }

  private def resolve(n: Arith3, m: Map[String, Long]): StateM[Unit] = for {
    arg12 <- resolve(n.arg1, m)
    arg22 <- resolve(n.arg2, m)
    dst2 <- resolve(n.dst, m)
    _ <- append(n match {
      case Arith3.Add(_, _, _) => Arith3.Add(arg12, arg22, dst2)
      case Arith3.Sub(_, _, _) => Arith3.Sub(arg12, arg22, dst2)
      case Arith3.Mul(_, _, _) => Arith3.Mul(arg12, arg22, dst2)
      case Arith3.Div(_, _, _) => Arith3.Div(arg12, arg22, dst2)
      case Arith3.Mod(_, _, _) => Arith3.Mod(arg12, arg22, dst2)
      case Arith3.Cmp(op, _, _, _) => Arith3.Cmp(op, arg12, arg22, dst2)
    })
  } yield ()

  private def resolve(n: Source, m: Map[String, Long]): StateM[Source] = n match {
    case Source.Loc(loc) => resolve(loc, m) >>| Source.Loc
    case _ => pure(n)
  }

  private def resolve(n: Location, m: Map[String, Long]): StateM[Location] = n match {
    case Location.Name(name) => m.get(name) match {
      case Some(offset) => pure(Location.Addr(Left(-offset), Some(rbp), None, 1L))
      case None => pure(Location.Addr(Right(name), None, None, 1L))
    }
    case Location.Array(base, Right(reg)) => resolve(base, m) >>= {
      case Location.Addr(offset, reg1, None, 1L) => pure(Location.Addr(offset, reg1, Some(reg), Lir.wordSize))
      case _ => throw new RuntimeException
    }
    case Location.Array(base, Left(name)) => for {
      base2 <- resolve(base, m)
      (offset, reg1) = base2 match {
        case Location.Addr(_offset, _reg1, None, 1L) => (_offset, _reg1)
        case _ => throw new RuntimeException
      }
      tName <- resolve(name, m)
      _ <- append(Copy.Mov(tName, rax))
    } yield Location.Addr(offset, reg1, Some(rax), Lir.wordSize)
    case Location.Addr(Right(name), reg1, reg2, scale) => m.get(name) match {
      case Some(offset) => pure(Location.Addr(Left(-offset), reg1, reg2, scale))
      case None => pure(n)
    }
    case _ => pure(n)
  }

}

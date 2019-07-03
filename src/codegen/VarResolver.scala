package codegen

import lir.Conversion._
import lir.Registers._
import lir._


/** Take an LIR and convert names to appropriate addressing */
object VarResolver {

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
      Method(name, Nil, Stack.Enter(size) :: initMem(size) ::: stmts.map(s => resolve(s, map)))
  }

  private def resolve(_n: Stmt, m: Map[String, Long]): Stmt = _n match {
    case n: Copy => resolve(n, m)
    case n: Stack => resolve(n, m)
    case n: Arith => resolve(n, m)
    case n: Control => n
  }

  private def resolve(n: Copy, m: Map[String, Long]): Copy = n match {
    case Copy.Lea(src, dst) => Copy.Lea(resolve(src, m), resolve(dst, m))
    case Copy.Mov(src, dst) => Copy.Mov(resolve(src, m), resolve(dst, m))
    case _ => n
  }

  private def resolve(n: Stack, m: Map[String, Long]): Stack = n match {
    case Stack.Push(src) => Stack.Push(resolve(src, m))
    case Stack.Pop(dst) => Stack.Pop(resolve(dst, m))
    case _ => n
  }

  private def resolve(n: Arith, m: Map[String, Long]): Arith = n match {
    case Arith.Add(src, dst) => Arith.Add(resolve(src, m), resolve(dst, m))
    case Arith.Sub(src, dst) => Arith.Sub(resolve(src, m), resolve(dst, m))
    case Arith.Imul(src, dst) => Arith.Imul(resolve(src, m), dst)
    case Arith.Idiv(divisor) => Arith.Idiv(resolve(divisor, m))
    case Arith.Ror(src, dst) => Arith.Ror(resolve(src, m), resolve(dst, m))
    case Arith.Cmp(src, dst) => Arith.Cmp(resolve(src, m), resolve(dst, m))
    case _ => n
  }

  private def resolve(n: Source, m: Map[String, Long]): Source = n match {
    case Source.Loc(loc) => Source.Loc(resolve(loc, m))
    case _ => n
  }

  private def resolve(n: Location, m: Map[String, Long]): Location = n match {
    case Location.Name(name) => m.get(name) match {
      case Some(offset) => Location.Addr(Left(-offset), Some(rbp), None, 1L)
      case None => Location.Addr(Right(name), Some(rip), None, 1L)
    }
    case Location.Addr(Right(name), reg1, reg2, scale) => m.get(name) match {
      case Some(offset) => Location.Addr(Left(-offset), reg1, reg2, scale)
      case None => n
    }
    case _ => n
  }

}

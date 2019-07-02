package codegen

import codegen.State._
import lir.Conversion._
import lir.Registers._
import lir._
import symboltable.{Descriptor, STScope, SymbolTable}
import util.State.{get, pure, put}

object LirGenerator {

  // We could have used (and have tried using) the listener model. However, it is less efficient and not that much
  // more concise than manual recursion

  type SymbolCtx = Map[ir.Ir, SymbolTable]

  def genLir(node: ir.Program, st: SymbolCtx): Program = gen(node, st)(initState)._2.program

  private def idToName(node: ir.Ir, id: ir.ID, st: SymbolCtx): String = st(node).lookup(id.name).get match {
    case Descriptor.Array(uid, _, _) => "ARR_" + id.name + "_" + uid
    case Descriptor.Variable(uid, _) => "VAR_" + id.name + "_" + uid
    case Descriptor.Method(_, _) => if (id.name == "main") id.name else "METHOD_" + id.name
    case Descriptor.Callout(_) => id.name
  }

  private def arraySize(node: ir.Ir, id: ir.ID, st: SymbolCtx): Long = st(node).lookup(id.name).get match {
    case Descriptor.Array(_, _, size) => size
    case _ => throw new RuntimeException
  }

  /** Label name for throwing out-of-bound error */
  private def throwOOB: String = ".THROW_OutOfBound"

  /** Label name for method falloff error */
  private def throwFalloff: String = ".THROW_Falloff"

  private def initState: Builder = Builder(Nil, Nil, Program(Nil, Nil, Nil), 0, isGlobal = true)

  private implicit def gen(node: ir.Program, st: SymbolCtx): BState[Unit] = for {
    strOOB <- declString("\"Array index out of bound\"")
    strFalloff <- declString("\"Method did not return\"")

    methods = List(
      Method(throwOOB, Nil, List(
        Copy.Lea(Location.Addr(Right(strOOB), Some(rip), None, 1L), rdi),
        Copy.Mov(0L, rax),
        Control.Call("printf"),
        Copy.Mov(-1L, rdi),
        Control.Call("exit"))),
      Method(throwFalloff, Nil, List(
        Copy.Lea(Location.Addr(Right(strFalloff), Some(rip), None, 1L), rdi),
        Copy.Mov(0L, rax),
        Control.Call("printf"),
        Copy.Mov(-2L, rdi),
        Control.Call("exit"))))

    s1 <- get
    _ <- put(s1.copy(program = s1.program.copy(methods = methods ::: s1.program.methods)))
    _ <- genList(node.fields, st)
    _ <- genList(node.methods, st)
    s2 <- get
    // Reverse method list so that the methods are in the same order as the source file
    _ <- put(s2.copy(program = s2.program.copy(methods = s2.program.methods.reverse)))
  } yield ()

  private implicit def gen(node: ir.FieldDecl, st: SymbolCtx): BState[Unit] =
    genList(node.ids, st) >> pure()

  private implicit def gen(node: ir.VarDecl, st: SymbolCtx): BState[Unit] = node match {
    case ir.VarDecl.IDDecl(id) => declVar(idToName(node, id, st))
    case ir.VarDecl.IDArrayDecl(id, size) => declArr(idToName(node, id, st), size.toLong)
  }

  private implicit def gen(node: ir.MethodDecl, st: SymbolCtx): BState[Unit] = for {
    _ <- toggleIsGlobal
    _ <- genList(node.params, st)
    _ <- gen(node.body, st)
    _ <- node.typ.typ match {
      case ir.VoidableType.VoidT => append(Stack.Leave) >> append(Control.Ret)
      case _ => append(Control.Jmp(throwFalloff))
    }
    s <- get
    // Since instruction lists are prepended, reverse them so that they are in the right order
    // Also, I don't know why "method = Method(..)" causes a compile error...
    method <- pure(Method(idToName(node, node.id, st), s.decls.reverse, s.code.reverse))
    _ <- put(s.copy(code = Nil, decls = Nil, program = s.program.copy(methods = method :: s.program.methods)))
    _ <- toggleIsGlobal
  } yield ()

  private implicit def gen(node: ir.ParamDecl, st: SymbolCtx): BState[Unit] = declVar(idToName(node, node.paramId, st))

  private implicit def gen(node: ir.Block, st: SymbolCtx): BState[Unit] =
    genList(node.fields, st) >> genList(node.stmts, st) >> pure()

  private def genList[A, B](nodes: List[A], st: SymbolCtx)(implicit gen: (A, SymbolCtx) => BState[B]): BState[List[B]] =
    nodes match {
      case n :: ns => for {
        t <- gen(n, st)
        ts <- genList(ns, st)
      } yield t :: ts
      case _ => pure(Nil)
    }

  /** Generate code for expression. Each expression 'returns' a simple name */
  private def gen(node: ir.Expr, st: SymbolCtx): BState[Location.Name] = node match {
    case n: ir.Expr.UnaryOp => gen(n, st)
    case n: ir.Expr.BinaryOp => gen(n, st)
    case n: ir.Expr.Ternary => for {
      p <- gen(n.p, st)
      t <- gen(n.t, st)
      f <- gen(n.f, st)
      tmp <- nextTmp
      _ <- append(Copy.Mov(p, r10))
      _ <- append(Arith.Cmp(1L, r10)) // check if p = 1
      _ <- append(Copy.Mov(t, r10))
      _ <- append(Copy.Mov(f, r11))
      _ <- append(Copy.Cmove(r10, r11))
      _ <- append(Copy.Mov(r11, tmp))
    } yield tmp
    case ir.Expr.Load(loc) => gen(loc, st)
    case ir.Expr.Call(call) => for {
      _ <- gen(call, st)
      t <- nextTmp
      _ <- append(Copy.Mov(rax, t))
    } yield t
    case ir.Expr.Length(id) => for {
      t <- nextTmp
      len = arraySize(node, id, st)
      _ <- append(Copy.Mov(len, t))
    } yield t
    case ir.Expr.LitInt(value) => nextTmp >>= (t => append(Copy.Mov(value.toLong, t)) >> pure(t))
    case ir.Expr.LitBool(value) => nextTmp >>= (t => append(Copy.Mov(if (value) 1L else 0L, t)) >> pure(t))
    case ir.Expr.LitChar(value) => nextTmp >>= (t => append(Copy.Mov(value.toLong, t)) >> pure(t))
  }

  private def gen(node: ir.Expr.UnaryOp, st: SymbolCtx): BState[Location.Name] = for {
    v <- gen(node.arg, st)
    t <- nextTmp
    _ <- append(Copy.Mov(v, r10))
    r11val = node.op match {
      case ir.Op.Bang() => 1L
      case ir.Op.Minus() => 0L
      case _ => throw new RuntimeException
    }
    _ <- append(Copy.Mov(r11val, r11))
    _ <- append(Arith.Sub(r10, r11))
    _ <- append(Copy.Mov(r11, t))
  } yield t

  private def gen(node: ir.Expr.BinaryOp, st: SymbolCtx): BState[Location.Name] = node.op match {
    case ir.Op.And() => genAndOr(node, st, isAnd = true)
    case ir.Op.Or() => genAndOr(node, st, isAnd = false)
    case ir.Op.Eqq() | ir.Op.Neq() | ir.Op.Lt() | ir.Op.Gt() | ir.Op.Lte() | ir.Op.Gte() => genCmp(node, st)
    case ir.Op.Plus() | ir.Op.Minus() | ir.Op.Times() => genPlusMinusTimes(node, st)
    case ir.Op.Div() | ir.Op.Mod() => genDiv(node, st)
    case _ => throw new RuntimeException
  }

  private def genAndOr(node: ir.Expr.BinaryOp, st: SymbolCtx, isAnd: Boolean): BState[Location.Name] = for {
    lEnd <- nextCount >>| ((if (isAnd) "AND_" else "OR_") + _ + "_end")
    arg1 <- gen(node.arg1, st)
    _ <- append(Copy.Mov(arg1, r10))
    _ <- append(Arith.Cmp(if (isAnd) 0L else 1L, r10))
    _ <- append(Control.Je(lEnd))
    arg2 <- gen(node.arg2, st)
    _ <- append(Copy.Mov(arg2, r10))
    _ <- append(Control.Label(lEnd))
    t <- nextTmp
    _ <- append(Copy.Mov(r10, t))
  } yield t

  private def genCmp(node: ir.Expr.BinaryOp, st: SymbolCtx): BState[Location.Name] = for {
    t <- nextTmp
    arg1 <- gen(node.arg1, st)
    arg2 <- gen(node.arg2, st)
    _ <- append(Copy.Mov(arg1, r10))
    _ <- append(Copy.Mov(arg2, r11))
    _ <- append(Arith.Cmp(r11, r10))
    _ <- append(Copy.Mov(0L, r10))
    _ <- append(Copy.Mov(1L, r11))
    _ <- append(node.op match {
      case ir.Op.Eqq() => Copy.Cmove(r11, r10)
      case ir.Op.Neq() => Copy.Cmovne(r11, r10)
      case ir.Op.Lt() => Copy.Cmovl(r11, r10)
      case ir.Op.Gt() => Copy.Cmovg(r11, r10)
      case ir.Op.Lte() => Copy.Cmovle(r11, r10)
      case ir.Op.Gte() => Copy.Cmovge(r11, r10)
      case _ => throw new RuntimeException
    })
    _ <- append(Copy.Mov(r10, t))
  } yield t

  private def genPlusMinusTimes(node: ir.Expr.BinaryOp, st: SymbolCtx): BState[Location.Name] = for {
    t <- nextTmp
    arg1 <- gen(node.arg1, st)
    arg2 <- gen(node.arg2, st)
    _ <- append(Copy.Mov(arg1, r10))
    _ <- append(Copy.Mov(arg2, r11))
    _ <- append(node.op match {
      case ir.Op.Plus() => Arith.Add(r11, r10)
      case ir.Op.Minus() => Arith.Sub(r11, r10)
      case ir.Op.Times() => Arith.Imul(r11, r10)
      case _ => throw new RuntimeException
    })
    _ <- append(Copy.Mov(r10, t))
  } yield t

  private def genDiv(node: ir.Expr.BinaryOp, st: SymbolCtx): BState[Location.Name] = for {
    t <- nextTmp
    arg1 <- gen(node.arg1, st)
    arg2 <- gen(node.arg2, st)
    _ <- append(Copy.Mov(0L, rdx))
    _ <- append(Copy.Mov(arg1, rax))
    _ <- append(Copy.Mov(arg2, r10))
    _ <- append(Arith.Idiv(r10))
    ret = node.op match {
      case ir.Op.Div() => rax
      case ir.Op.Mod() => rdx
      case _ => throw new RuntimeException
    }
    _ <- append(Copy.Mov(ret, t))
  } yield t

  private implicit def gen(node: ir.Stmt, st: SymbolCtx): BState[Unit] = node match {
    case n: ir.Stmt.Assign => genAssign(node, n.location, n.e, st)
    case n: ir.Stmt.PlusAssign => genAssign(node, n.location, n.e, st)
    case n: ir.Stmt.MinusAssign => genAssign(node, n.location, n.e, st)
    case n: ir.Stmt.Cond => gen(n, st)
    //    case n: ir.Stmt.For => gen(n, st)
    //    case ir.Stmt.While(cond, _) =>
    //    case ir.Stmt.Return(None) => getMethodType(t) match {
    //    case ir.Stmt.Return(_) => getMethodType(t) match {
    case _ => pure()
  }

  private def genAssign(node: ir.Stmt, loc: ir.Location, e: ir.Expr, st: SymbolCtx): BState[Unit] = for {
    l <- gen(loc, st)
    r <- gen(e, st)
    _ <- append(Copy.Mov(l, r10))
    _ <- append(Copy.Mov(r, r11))
    _ <- node match {
      case _: ir.Stmt.Assign => append(Copy.Mov(r11, r10))
      case _: ir.Stmt.PlusAssign => append(Arith.Add(r11, r10))
      case _: ir.Stmt.MinusAssign => append(Arith.Sub(r11, r10))
      case _ => throw new RuntimeException
    }
    _ <- append(Copy.Mov(r10, l))
  } yield ()

  private def gen(node: ir.Stmt.Cond, st: SymbolCtx): BState[Unit] = for {
    p <- gen(node.p, st)
    lTrue <- nextCount >>| ("IF_" + _ + "_true")
    lEnd <- nextCount >>| ("IF_" + _ + "_end")
    _ <- append(Copy.Mov(p, r10))
    _ <- append(Arith.Cmp(1L, r10))
    _ <- append(Control.Je(lTrue))
    _ <- node.f match {
      case Some(f) => gen(f, st)
      case None => append(Control.Nop)
    }
    _ <- append(Control.Jmp(lEnd))
    _ <- append(Control.Label(lTrue))
    _ <- gen(node.t, st)
    _ <- append(Control.Label(lEnd))
  } yield ()

  private def gen(node: ir.Location, st: SymbolCtx): BState[Location.Name] = node match {
    case ir.Location.Var(id) => pure(idToName(node, id, st))
    case ir.Location.Cell(id, index) => for {
      tIndex <- gen(index, st)

      // Bound checking
      size = arraySize(node, id, st)
      _ <- append(Copy.Mov(size, r10))
      _ <- append(Copy.Mov(tIndex, r11))
      _ <- append(Arith.Cmp(r11, r10))
      _ <- append(Copy.Mov(0L, r10))
      _ <- append(Copy.Mov(1L, r11))
      _ <- append(Copy.Cmovle(r11, r10))
      _ <- append(Arith.Cmp(1L, r10))
      _ <- append(Control.Je(throwOOB))

      // Load index to %r10
      _ <- append(Copy.Mov(tIndex, r10))

      // If local:
      //   mov    offset(%rbp, %10, wordsize), %r10
      // If global:
      //   lea    label(%rip), %r11
      //   mov    (%r11, %r10, wordsize), %r10
      _ <- st(node).findScope(id.name) match {
        case Some(STScope.Global) =>
          append(Copy.Lea(Location.Addr(Right(idToName(node, id, st)), Some(rip), None, 1L), r11)) >>
            append(Copy.Mov(Location.Addr(Left(0L), Some(r11), Some(r10), wordSize), r10))
        case Some(_) =>
          append(Copy.Mov(Location.Addr(Right(idToName(node, id, st)), Some(rbp), Some(r10), wordSize), r10))
        case _ => throw new RuntimeException
      }

      t <- nextTmp
      _ <- append(Copy.Mov(r10, t))
    } yield t
  }

  private def gen(node: ir.MethodCall, st: SymbolCtx): BState[Unit] = for {
    args <- genList(node.args, st)
    _ <- genPushArgs(args, 0)
    _ <- append(Control.Call(idToName(node, node.method, st)))
  } yield ()

  private implicit def gen(node: ir.MethodArg, st: SymbolCtx): BState[Location.Name] = node match {
    case ir.MethodArg.ExprArg(e) => gen(e, st)
    case ir.MethodArg.StringArg(s) => declString(s) >>| Location.Name
  }

  private def genPushArgs(args: List[Location.Name], i: Int): BState[Unit] = (args, i) match {
    case (x :: xs, 0) => append(Copy.Mov(x, rdi)) >> genPushArgs(xs, i + 1)
    case (x :: xs, 1) => append(Copy.Mov(x, rsi)) >> genPushArgs(xs, i + 1)
    case (x :: xs, 2) => append(Copy.Mov(x, rdx)) >> genPushArgs(xs, i + 1)
    case (x :: xs, 3) => append(Copy.Mov(x, rcx)) >> genPushArgs(xs, i + 1)
    case (x :: xs, 4) => append(Copy.Mov(x, r8))  >> genPushArgs(xs, i + 1)
    case (x :: xs, 5) => append(Copy.Mov(x, r9))  >> genPushArgs(xs.reverse, i + 1)
    case (x :: xs, _) => append(Stack.Push(x))    >> genPushArgs(xs, i + 1)
    case _ => pure()
  }
}

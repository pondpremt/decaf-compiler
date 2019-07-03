package codegen

import codegen.State._
import lir.Conversion._
import lir.Registers._
import lir._
import symboltable.{Descriptor, STScope, SymbolTable}
import util.State.{get, pure, put, update}

object LirGenerator {

  // We could have used (and have tried using) the listener model. However, it is less efficient and not that much
  // more concise than manual recursion

  type SymbolCtx = Map[ir.Ir.UID, SymbolTable]

  def genLir(node: ir.Program, st: SymbolCtx): Program = gen(node, st)(initState)._2.program

  private def idToName(id: ir.ID, st: SymbolCtx): String = st(id.uid).lookup(id.name).get match {
    case d: Descriptor.Array => "ARR_" + id.name + "_" + d.uid
    case d: Descriptor.Variable => "VAR_" + id.name + "_" + d.uid
    case _: Descriptor.Method => if (id.name == "main") id.name else "METHOD_" + id.name
    case _: Descriptor.Callout => id.name
  }

  private def arraySize(id: ir.ID, st: SymbolCtx): Long = st(id.uid).lookup(id.name).get match {
    case Descriptor.Array(_, size) => size
    case _ => throw new RuntimeException
  }

  /** Label name for throwing out-of-bound error */
  private def throwOOB: String = ".THROW_OutOfBound"

  /** Label name for method falloff error */
  private def throwFalloff: String = ".THROW_Falloff"

  private def initState: Builder = Builder(Nil, Nil, Program(Nil, Nil, Nil),
    counter = 0, isGlobal = true, break = "", continue = "")

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

    _ <- update((s: Builder) => s.copy(program = s.program.copy(methods = methods ::: s.program.methods)))
    _ <- genList(node.fields, st)
    _ <- genList(node.methods, st)
    // Reverse method and string lists so that they show up in the same order as the source file
    _ <- update((s: Builder) => s.copy(program = s.program.copy(
      methods = s.program.methods.reverse,
      strings = s.program.strings.reverse)))
  } yield ()

  private implicit def gen(node: ir.FieldDecl, st: SymbolCtx): BState[Unit] =
    genList(node.ids, st) >> pure()

  private implicit def gen(node: ir.VarDecl, st: SymbolCtx): BState[Unit] = node match {
    case ir.VarDecl.IDDecl(id) => declVar(idToName(id, st))
    case ir.VarDecl.IDArrayDecl(id, size) => declArr(idToName(id, st), size.toLong)
  }

  private implicit def gen(node: ir.MethodDecl, st: SymbolCtx): BState[Unit] = for {
    _ <- toggleIsGlobal
    _ <- genParams(node.params, st, 0)
    _ <- gen(node.body, st)
    _ <- node.typ.typ match {
      case ir.VoidableType.VoidT => append(Stack.Leave) >> append(Control.Ret)
      case _ => append(Control.Jmp(throwFalloff))
    }
    s <- get
    // Since instruction lists are prepended, reverse them so that they are in the right order
    // Also, I don't know why "method = Method(..)" causes a compile error...
    method <- pure(Method(idToName(node.id, st), s.decls.reverse, s.code.reverse))
    _ <- put(s.copy(code = Nil, decls = Nil, program = s.program.copy(methods = method :: s.program.methods)))
    _ <- toggleIsGlobal
  } yield ()

  private def genParams(decls: List[ir.ParamDecl], st: SymbolCtx, i: Int): BState[Unit] = decls match {
    case Nil => append(Control.Nop)
    case x :: xs =>
      val name = idToName(x.paramId, st)
      for {
        _ <- declVar(name)
        _ <- i match {
          case 0 => append(Copy.Mov(rdi, name))
          case 1 => append(Copy.Mov(rsi, name))
          case 2 => append(Copy.Mov(rdx, name))
          case 3 => append(Copy.Mov(rcx, name))
          case 4 => append(Copy.Mov(r8, name))
          case 5 => append(Copy.Mov(r9, name))
          case n =>
            append(Copy.Mov(Location.Addr(Left((n - 4) * Lir.wordSize), Some(rbp), None, 1L), r10)) >>
              append(Copy.Mov(r10, name))
        }
        _ <- genParams(xs, st, i + 1)
      } yield ()
  }

  private implicit def gen(node: ir.Block, st: SymbolCtx): BState[Unit] =
    genList(node.fields, st) >> genList(node.stmts, st) >> pure()

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
    case n: ir.Expr.Load => gen(n, st)
    case ir.Expr.Call(call) => for {
      _ <- gen(call, st)
      t <- nextTmp
      _ <- append(Copy.Mov(rax, t))
    } yield t
    case ir.Expr.Length(id) => for {
      t <- nextTmp
      len = arraySize(id, st)
      _ <- append(Copy.Mov(len, t))
    } yield t
    case ir.Expr.LitInt(value) => nextTmp >>= (t => append(Copy.Mov(value.toLong, t)) >> pure(t))
    case ir.Expr.LitBool(value) => nextTmp >>= (t => append(Copy.Mov(if (value) 1L else 0L, t)) >> pure(t))
    case ir.Expr.LitChar(value) => nextTmp >>= (t => append(Copy.Mov(value.toLong, t)) >> pure(t))
  }

  private def gen(node: ir.Expr.Load, st: SymbolCtx): BState[Location.Name] = node match {
    case ir.Expr.Load(ir.Location.Var(id)) => pure(idToName(id, st))
    case ir.Expr.Load(cell: ir.Location.Cell) => for {
      tLoc <- genCellLoc(cell, st)
      _ <- append(Copy.Mov(tLoc, r10))
      t <- nextTmp
      _ <- append(Copy.Mov(r10, t))
    } yield t
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
    lEnd <- nextCount >>| ((if (isAnd) ".and_" else ".or_") + _ + "_end")
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
    case n: ir.Stmt.CallStmt => gen(n.call, st)
    case n: ir.Stmt.For => gen(n, st)
    case n: ir.Stmt.While => gen(n, st)
    case _: ir.Stmt.Break => get >>= (s => append(Control.Jmp(s.break)))
    case _: ir.Stmt.Continue => get >>= (s => append(Control.Jmp(s.continue)))
    case n: ir.Stmt.Return => gen(n, st)
  }

  private def genAssign(node: ir.Stmt, loc: ir.Location, e: ir.Expr, st: SymbolCtx): BState[Unit] = for {
    // TODO may need some optimization when it comes to arrays
    // l is var name for var, or temp storing addr for array
    l <- loc match {
      case ir.Location.Var(id) => pure[Builder, Location.Name](idToName(id, st))
      case cell: ir.Location.Cell => for {
        addr <- genCellLoc(cell, st)
        _ <- append(Copy.Lea(addr, r10))
        t <- nextTmp
        _ <- append(Copy.Mov(r10, t))
      } yield t
    }
    // r is name for value o RHS
    r <- gen(e, st)
    // r10 is value of LHS
    _ <- loc match {
      case _: ir.Location.Var => append(Copy.Mov(l, r10))
      case _: ir.Location.Cell => for {
        t <- nextTmp
        _ <- append(Copy.Mov(l, r11))
        _ <- append(Copy.Mov(Location.Addr(Left(0L), Some(r11), None, 1L), r10))
      } yield t
    }
    // r11 is value of RHS
    _ <- append(Copy.Mov(r, r11))
    // r10 is value to be assigned
    _ <- node match {
      case _: ir.Stmt.Assign => append(Copy.Mov(r11, r10))
      case _: ir.Stmt.PlusAssign => append(Arith.Add(r11, r10))
      case _: ir.Stmt.MinusAssign => append(Arith.Sub(r11, r10))
      case _ => throw new RuntimeException
    }
    // assign to l (if name) or address in l (if array)
    _ <- loc match {
      case _: ir.Location.Var => append(Copy.Mov(r10, l))
      case _: ir.Location.Cell => for {
        t <- nextTmp
        _ <- append(Copy.Mov(l, r11))
        _ <- append(Copy.Mov(r10, Location.Addr(Left(0L), Some(r11), None, 1L)))
      } yield t
    }
  } yield ()

  private def gen(node: ir.Stmt.Cond, st: SymbolCtx): BState[Unit] = for {
    p <- gen(node.p, st)
    lTrue <- nextCount >>| (".if_" + _ + "_true")
    lEnd <- getCount >>| (".if_" + _ + "_end")
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

  private def gen(node: ir.Stmt.While, st: SymbolCtx): BState[Unit] = for {
    lCond <- nextCount >>| (".while_" + _ + "_cond")
    lTerminal <- getCount >>| (".while_" + _ + "_terminal")
    _ <- append(Control.Label(lCond))
    p <- gen(node.cond, st)
    _ <- append(Copy.Mov(p, r10))
    _ <- append(Arith.Cmp(1L, r10))
    _ <- append(Control.Jne(lTerminal))
    // Change break/continur jump targets when translating the body
    sBefore <- get
    _ <- put(sBefore.copy(break = lTerminal, continue = lCond))
    _ <- gen(node.body, st)
    _ <- update((s: Builder) => s.copy(break = sBefore.break, continue = sBefore.continue))
    _ <- append(Control.Jmp(lCond))
    _ <- append(Control.Label(lTerminal))
    // Change the targets back
  } yield ()

  private def gen(node: ir.Stmt.For, st: SymbolCtx): BState[Unit] = for {
    lCond <- nextCount >>| (".for_" + _ + "_cond")
    lEnd <- getCount >>| (".for_" + _ + "_end")
    lTerminal <- getCount >>| (".for_" + _ + "_terminal")
    i = idToName(node.index, st)
    start <- gen(node.start, st)
    stop <- gen(node.stop, st)
    _ <- append(Copy.Mov(start, r10))
    _ <- append(Copy.Mov(r10, i))
    _ <- append(Control.Label(lCond))
    _ <- append(Copy.Mov(i, r10))
    _ <- append(Arith.Cmp(stop, r10))
    _ <- append(Control.Jge(lTerminal))
    sBefore <- get
    _ <- put(sBefore.copy(break = lTerminal, continue = lEnd))
    _ <- gen(node.body, st)
    _ <- update((s: Builder) => s.copy(break = sBefore.break, continue = sBefore.continue))
    _ <- append(Control.Label(lEnd))
    _ <- append(Arith.Add(node.step.toLong, i))
    _ <- append(Control.Jmp(lCond))
    _ <- append(Control.Label(lTerminal))
  } yield ()

  private def gen(node: ir.Stmt.Return, st: SymbolCtx): BState[Unit] = for {
    _ <- node.value match {
      case Some(e) => gen(e, st) >>= (t => append(Copy.Mov(t, rax)))
      case _ => append(Control.Nop)
    }
    _ <- append(Stack.Leave)
    _ <- append(Control.Ret)
  } yield ()

  private def genCellLoc(node: ir.Location.Cell, st: SymbolCtx): BState[Location.Addr] = for {
    tIndex <- gen(node.index, st)

    // Bound checking
    size = arraySize(node.id, st)
    _ <- append(Copy.Mov(size, r10))
    _ <- append(Copy.Mov(tIndex, r11))
    _ <- append(Arith.Cmp(r10, r11))
    _ <- append(Control.Jge(throwOOB))
    _ <- append(Arith.Cmp(0L, r11))
    _ <- append(Control.Jl(throwOOB))

    // If local:
    //   offset(%rbp, %10, wordsize)
    // If global:
    //   lea    label(%rip), %r11
    //   (%r11, %r10, wordsize)
    isGlobal = st(node.uid).findScope(node.id.name) match {
      case Some(STScope.Global) => true
      case _ => false
    }
    _ <- append(Copy.Mov(tIndex, r10))
    _ <-
    if (isGlobal) append(Copy.Lea(Location.Addr(Right(idToName(node.id, st)), Some(rip), None, 1L), r11))
    else append(Control.Nop)
  } yield if (isGlobal)
    Location.Addr(Left(0L), Some(r11), Some(r10), Lir.wordSize)
  else
    Location.Addr(Right(idToName(node.id, st)), Some(rbp), Some(r10), Lir.wordSize)

  private def gen(node: ir.MethodCall, st: SymbolCtx): BState[Unit] = for {
    args <- genList(node.args, st)
    _ <- genPushArgs(args, 0)
    _ <- if (node.method.name == "printf") append(Copy.Mov(0L, rax)) else append(Control.Nop)
    _ <- append(Control.Call(idToName(node.method, st)))
  } yield ()

  private implicit def gen(node: ir.MethodArg, st: SymbolCtx): BState[Location.Name] = node match {
    case ir.MethodArg.ExprArg(e) => gen(e, st)
    case ir.MethodArg.StringArg(s) => for {
      name <- declString(s) >>| Location.Name
      t <- nextTmp
      _ <- append(Copy.Lea(name, r10))
      _ <- append(Copy.Mov(r10, t))
    } yield t
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

  private def genList[A, B](nodes: List[A], st: SymbolCtx)(implicit gen: (A, SymbolCtx) => BState[B]): BState[List[B]] =
    nodes match {
      case n :: ns => for {
        t <- gen(n, st)
        ts <- genList(ns, st)
      } yield t :: ts
      case _ => pure(Nil)
    }

}

package codegen

import codegen.LirGeneratorState._
import lir.Conversion._
import lir.Registers._
import lir._
import symboltable.{Descriptor, SymbolTable}
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
    strOOB <- declString("\"Array index out of bound\"") >>| Source.Str
    strFalloff <- declString("\"Method did not return\"") >>| Source.Str

    methods = List(
      Method(throwOOB, Nil, List(
        Copy.Mov(strOOB, rdi),
        Copy.Mov(0L, rax),
        Control.Call("printf"),
        Copy.Mov(-1L, rdi),
        Control.Call("exit"))),
      Method(throwFalloff, Nil, List(
        Copy.Mov(strFalloff, rdi),
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
          case n => append(Copy.Mov(Location.Addr(Left((n - 4) * Lir.wordSize), Some(rbp), None, 1L), name))
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
      lEnd <- nextCount >>| (".ternary_" + _ + "_end")
      tmp <- nextTmp
      _ <- append(Arith.Cmp(0L, p))
      _ <- append(Copy.Mov(f, tmp))
      _ <- append(Control.Cjmp(CmpOp.E, lEnd))
      _ <- append(Copy.Mov(t, tmp))
      _ <- append(Control.Label(lEnd))
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
      t <- nextTmp
      _ <- append(Copy.Mov(tLoc, t))
    } yield t
  }

  private def gen(node: ir.Expr.UnaryOp, st: SymbolCtx): BState[Location.Name] = for {
    v <- gen(node.arg, st)
    t <- nextTmp
    base = node.op match {
      case ir.Op.Bang() => 1L
      case ir.Op.Minus() => 0L
      case _ => throw new RuntimeException
    }
    _ <- append(Arith3.Sub(base, v, t))
  } yield t

  private def gen(node: ir.Expr.BinaryOp, st: SymbolCtx): BState[Location.Name] = node.op match {
    case ir.Op.And() => genAndOr(node, st, isAnd = true)
    case ir.Op.Or() => genAndOr(node, st, isAnd = false)
    case ir.Op.Eqq() | ir.Op.Neq() | ir.Op.Lt() | ir.Op.Gt() | ir.Op.Lte() | ir.Op.Gte() => genCmp(node, st)
    case ir.Op.Plus() | ir.Op.Minus() | ir.Op.Times() | ir.Op.Div() | ir.Op.Mod() => genArith(node, st)
    case _ => throw new RuntimeException
  }

  private def genAndOr(node: ir.Expr.BinaryOp, st: SymbolCtx, isAnd: Boolean): BState[Location.Name] = for {
    lEnd <- nextCount >>| ((if (isAnd) ".and_" else ".or_") + _ + "_end")
    t <- nextTmp
    arg1 <- gen(node.arg1, st)
    _ <- append(Copy.Mov(arg1, t))
    _ <- append(Arith.Cmp(if (isAnd) 0L else 1L, t))
    _ <- append(Control.Cjmp(CmpOp.E, lEnd))
    arg2 <- gen(node.arg2, st)
    _ <- append(Copy.Mov(arg2, t))
    _ <- append(Control.Label(lEnd))
  } yield t

  private def genCmp(node: ir.Expr.BinaryOp, st: SymbolCtx): BState[Location.Name] = for {
    t <- nextTmp
    arg1 <- gen(node.arg1, st)
    arg2 <- gen(node.arg2, st)
    op = node.op match {
      case ir.Op.Eqq() => CmpOp.E
      case ir.Op.Neq() => CmpOp.Ne
      case ir.Op.Lt() => CmpOp.L
      case ir.Op.Gt() => CmpOp.G
      case ir.Op.Lte() => CmpOp.Le
      case ir.Op.Gte() => CmpOp.Ge
      case _ => throw new RuntimeException
    }
    _ <- append(Arith3.Cmp(op, arg1, arg2, t))
  } yield t

  private def genArith(node: ir.Expr.BinaryOp, st: SymbolCtx): BState[Location.Name] = for {
    t <- nextTmp
    arg1 <- gen(node.arg1, st)
    arg2 <- gen(node.arg2, st)
    _ <- append(node.op match {
      case ir.Op.Plus() => Arith3.Add(arg1, arg2, t)
      case ir.Op.Minus() => Arith3.Sub(arg1, arg2, t)
      case ir.Op.Times() => Arith3.Mul(arg1, arg2, t)
      case ir.Op.Div() => Arith3.Div(arg1, arg2, t)
      case ir.Op.Mod() => Arith3.Mod(arg1, arg2, t)
      case _ => throw new RuntimeException
    })
  } yield t

  private implicit def gen(node: ir.Stmt, st: SymbolCtx): BState[Unit] = node match {
    case n: ir.Stmt.Assign => genAssign(n, st)
    case n: ir.Stmt.PlusAssign => genAugmentedAssign(node, n.location, n.e, st)
    case n: ir.Stmt.MinusAssign => genAugmentedAssign(node, n.location, n.e, st)
    case n: ir.Stmt.Cond => gen(n, st)
    case n: ir.Stmt.CallStmt => gen(n.call, st)
    case n: ir.Stmt.For => gen(n, st)
    case n: ir.Stmt.While => gen(n, st)
    case _: ir.Stmt.Break => get >>= (s => append(Control.Jmp(s.break)))
    case _: ir.Stmt.Continue => get >>= (s => append(Control.Jmp(s.continue)))
    case n: ir.Stmt.Return => gen(n, st)
  }

  private def genAssign(node: ir.Stmt.Assign, st: SymbolCtx): BState[Unit] = for {
    loc <- node.location match {
      case ir.Location.Var(id) => pure[Builder, Location.Name](idToName(id, st))
      case cell: ir.Location.Cell => genCellLoc(cell, st)
    }
    e <- gen(node.e, st)
    _ <- append(Copy.Mov(e, loc))
  } yield ()

  private def genAugmentedAssign(node: ir.Stmt, loc: ir.Location, e: ir.Expr, st: SymbolCtx): BState[Unit] = for {
    locAddr <- loc match {
      case ir.Location.Var(id) => pure[Builder, Location.Name](idToName(id, st))
      case cell: ir.Location.Cell => genCellLoc(cell, st)
    }
    tLoc <- nextTmp
    _ <- append(Copy.Mov(locAddr, tLoc))
    tE <- gen(e, st)
    _ <- append(node match {
      case _: ir.Stmt.PlusAssign => Arith3.Add(tLoc, tE, locAddr)
      case _: ir.Stmt.MinusAssign => Arith3.Sub(tLoc, tE, locAddr)
      case _ => throw new RuntimeException
    })
  } yield ()

  private def gen(node: ir.Stmt.Cond, st: SymbolCtx): BState[Unit] = for {
    p <- gen(node.p, st)
    lTrue <- nextCount >>| (".if_" + _ + "_true")
    lEnd <- getCount >>| (".if_" + _ + "_end")
    _ <- append(Arith.Cmp(1L, p))
    _ <- append(Control.Cjmp(CmpOp.E, lTrue))
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
    _ <- append(Arith.Cmp(1L, p))
    _ <- append(Control.Cjmp(CmpOp.Ne, lTerminal))
    // Change break/continue jump targets when translating the body
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
    _ <- append(Copy.Mov(start, i))
    _ <- append(Control.Label(lCond))
    _ <- append(Arith.Cmp(stop, i))
    _ <- append(Control.Cjmp(CmpOp.Ge, lTerminal))
    sBefore <- get
    _ <- put(sBefore.copy(break = lTerminal, continue = lEnd))
    _ <- gen(node.body, st)
    _ <- update((s: Builder) => s.copy(break = sBefore.break, continue = sBefore.continue))
    _ <- append(Control.Label(lEnd))
    _ <- append(Arith3.Add(i, node.step.toLong, i))
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

  private def genCellLoc(node: ir.Location.Cell, st: SymbolCtx): BState[Location.Array] = for {
    // Bound checking
    tIndex <- gen(node.index, st)
    size = arraySize(node.id, st)
    _ <- append(Arith.Cmp(size, tIndex))
    _ <- append(Control.Cjmp(CmpOp.Ge, throwOOB))
    _ <- append(Arith.Cmp(0L, tIndex))
    _ <- append(Control.Cjmp(CmpOp.L, throwOOB))
  } yield Location.Array(idToName(node.id, st), Left(tIndex))

  private def gen(node: ir.MethodCall, st: SymbolCtx): BState[Unit] = for {
    args <- genList(node.args, st)
    _ <- genPushArgs(args, 0)
    _ <- if (node.method.name == "printf") append(Copy.Mov(0L, rax)) else append(Control.Nop)
    _ <- append(Control.Call(idToName(node.method, st)))
  } yield ()

  private implicit def gen(node: ir.MethodArg, st: SymbolCtx): BState[Location.Name] = node match {
    case ir.MethodArg.ExprArg(e) => gen(e, st)
    case ir.MethodArg.StringArg(s) => for {
      name <- declString(s) >>| Source.Str
      t <- nextTmp
      _ <- append(Copy.Mov(name, t))
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

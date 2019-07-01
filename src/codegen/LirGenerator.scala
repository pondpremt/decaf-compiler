package codegen

import codegen.State._
import lir.Conversion._
import lir._
import lir.Registers._
import symboltable.{Descriptor, SymbolTable}
import util.State.{get, put, pure}

object LirGenerator {

  // We could have used (and have tried using) the listener model. However, it is less efficient and not that much
  // more concise than manual recursion

  type SymbolCtx = Map[ir.Ir, SymbolTable]

  def idToName(node: ir.Ir, id: ir.ID, st: SymbolCtx): String = st(node).lookup(id.name).get match {
    case Descriptor.Array(uid, _, _) => "ARR_" + id.name + "_" + uid
    case Descriptor.Variable(uid, _) => "VAR_" + id.name + "_" + uid
    case Descriptor.Method(uid, _) => if (id.name == "main") id.name else "METHOD_" + id.name + "_" + uid
    case Descriptor.Callout(_) => id.name
  }

  def arraySize(node: ir.Ir, id: ir.ID, st: SymbolCtx): Long = st(node).lookup(id.name).get match {
    case Descriptor.Array(_, _, size) => size
    case _ => throw new RuntimeException
  }

  def strName(name: String): String = ".STR_" + name

  def throwOutOfBound = Method("THROW_OutOfBound", Nil, List(
    Copy.Mov(strName("SYS_OutOfBound"), rdi),
    Copy.Mov(0L, rax),
    Control.Call("printf"),
    Copy.Mov(60L, rax),
    Copy.Mov(-1L, rdi),
    Control.Syscall()
  ))

  def throwFalloff = Method("THROW_Falloff", Nil, List(
    Copy.Mov(strName("SYS_Falloff"), rdi),
    Copy.Mov(0L, rax),
    Control.Call("printf"),
    Copy.Mov(60L, rax),
    Copy.Mov(-2L, rdi),
    Control.Syscall()
  ))

  def sysStrings = List(
    Str(strName("SYS_OutOfBound"), "Array index out of bound"),
    Str(strName("SYS_Falloff"), "Method did not return"))

  def sysMethods = List(throwOutOfBound, throwFalloff)

  def initState: Builder = Builder(Nil, Nil, Program(Nil, sysStrings, sysMethods), 0, isGlobal = true)

  // def genLir(node: ir.Program, st: Map[ir.Ir, SymbolTable]:

  implicit def gen(node: ir.Program, st: SymbolCtx): BState[Unit] =
    genList(node.fields, st) >> genList(node.methods, st) >> pure()

  implicit def gen(node: ir.FieldDecl, st: SymbolCtx): BState[Unit] =
    genList(node.ids, st) >> pure()

  implicit def gen(node: ir.VarDecl, st: SymbolCtx): BState[Unit] = node match {
    case ir.VarDecl.IDDecl(id) => declVar(idToName(node, id, st))
    case ir.VarDecl.IDArrayDecl(id, size) => declArr(idToName(node, id, st), size.toLong)
  }

  implicit def gen(node: ir.MethodDecl, st: SymbolCtx): BState[Unit] = for {
    _ <- toggleIsGlobal
    _ <- genList(node.params, st)
    _ <- gen(node.body, st)
    _ <- node.typ.typ match {
      case ir.VoidableType.VoidT => append(Stack.Leave) >> append(Control.Ret)
      case _ => append(Control.Jmp(throwFalloff.name))
    }
    s <- get
    method = Method(idToName(node, node.id, st), s.decls, s.code)
    _ <- put(s.copy(code = Nil, decls = Nil, program = s.program.copy(methods = method :: s.program.methods)))
  } yield ()

  implicit def gen(node: ir.ParamDecl, st: SymbolCtx): BState[Unit] = declVar(idToName(node, node.paramId, st))

  implicit def gen(node: ir.Block, st: SymbolCtx): BState[Unit] =
    genList(node.fields, st) >> genList(node.stmts, st) >> pure()

  implicit def gen(node: ir.Stmt, st: SymbolCtx): BState[Unit] = pure()

  def genList[A, B](nodes: List[A], st: SymbolCtx)(implicit gen: (A, SymbolCtx) => BState[B]): BState[List[B]] =
    nodes match {
      case n :: ns => for {
        t <- gen(n, st)
        ts <- genList(ns, st)
      } yield t :: ts
      case _ => pure(Nil)
    }

  /** Generate code for expression. Each expression 'returns' a simple name */
  def gen(node: ir.Expr, st: SymbolCtx): BState[Location.Name] = node match {
    case n: ir.Expr.UnaryOp => gen(n, st)
    case n: ir.Expr.BinaryOp => gen(n, st)
    case n: ir.Expr.Ternary => for {
      p <- gen(n.p, st)
      t <- gen(n.t, st)
      f <- gen(n.f, st)
      tmp <- nextTmp
      _ <- append(Arith.Cmp(1L, p)) // check if p = 1
      _ <- append(Copy.Mov(t, r10))
      _ <- append(Copy.Mov(f, r11))
      _ <- append(Copy.Cmove(r10, r11))
      _ <- append(Copy.Mov(r11, tmp))
    } yield tmp
    case ir.Expr.Load(loc) => for {
      l <- gen(loc, st)
      t <- nextTmp
      _ <- append(Copy.Mov(l, t))
    } yield t
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

  def gen(node: ir.Expr.UnaryOp, st: SymbolCtx): BState[Location.Name] = for {
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

  def gen(node: ir.Expr.BinaryOp, st: SymbolCtx): BState[Location.Name] = node.op match {
    case ir.Op.And() => genAndOr(node, st, isAnd = true)
    case ir.Op.Or() => genAndOr(node, st, isAnd = false)
    case ir.Op.Eqq() | ir.Op.Neq() | ir.Op.Lt() | ir.Op.Gt() | ir.Op.Lte() | ir.Op.Gte() => genCmp(node, st)
    case ir.Op.Plus() | ir.Op.Minus() | ir.Op.Times() => genPlusMinusTimes(node, st)
    case ir.Op.Div() | ir.Op.Mod() => genDiv(node, st)
    case _ => throw new RuntimeException
  }

  def genAndOr(node: ir.Expr.BinaryOp, st: SymbolCtx, isAnd: Boolean): BState[Location.Name] = for {
    lEnd <- nextCount >>| ((if (isAnd) "AND_" else "OR_") + _ + "_end")
    arg1 <- gen(node.arg1, st)
    _ <- append(Copy.Mov(arg1, r10))
    _ <- append(Arith.Cmp(if (isAnd) 0L else 1L, r10))
    _ <- append(Control.Je(lEnd))
    arg2 <- gen(node.arg2, st)
    _ <- append(Copy.Mov(arg2, r10))
    _ <- append(Label(lEnd))
    t <- nextTmp
    _ <- append(Copy.Mov(r10, t))
  } yield t

  def genCmp(node: ir.Expr.BinaryOp, st: SymbolCtx): BState[Location.Name] = for {
    t <- nextTmp
    arg1 <- gen(node.arg1, st)
    arg2 <- gen(node.arg2, st)
    _ <- append(Copy.Mov(arg1, r10))
    _ <- append(Copy.Mov(arg2, r11))
    _ <- append(Arith.Cmp(r10, r11))
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

  def genPlusMinusTimes(node: ir.Expr.BinaryOp, st: SymbolCtx): BState[Location.Name] = for {
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

  def genDiv(node: ir.Expr.BinaryOp, st: SymbolCtx): BState[Location.Name] = for {
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

  implicit def gen(node: ir.Stmt, st: SymbolCtx): BState[Unit] = node match {
    case n: ir.Stmt.Assign => genAssign(node, n.location, n.e, st)
    case n: ir.Stmt.PlusAssign => genAssign(node, n.location, n.e, st)
    case n: ir.Stmt.MinusAssign => genAssign(node, n.location, n.e, st)
    //    case ir.Stmt.Cond(p, _, _) =>
    //    case ir.Stmt.For(_, start, stop, _, _) =>
    //    case ir.Stmt.While(cond, _) =>
    //    case ir.Stmt.Return(None) => getMethodType(t) match {
    //    case ir.Stmt.Return(_) => getMethodType(t) match {
    //    case _ => Result.Good(t)
  }

  def genAssign(node: ir.Stmt, loc: ir.Location, e: ir.Expr, st: SymbolCtx): BState[Unit] = for {
    l <- gen(loc, st)
    r <- gen(e, st)
    _ <- append(Copy.Mov(l, r10))
    _ <- append(Copy.Mov(r, r11))
    _ <- node match {
      case _: ir.Stmt.Assign => append(Copy.Mov(r11, r10))
      case _: ir.Stmt.PlusAssign => append(Arith.Add(r11, r10))
      case _: ir.Stmt.MinusAssign => append(Arith.Sub(r11, r10))
    }
    _ <- append(Copy.Mov(r10, l))
  } yield ()

  def gen(node: ir.Location, st: SymbolCtx): BState[Location.Name] = node match {
    case ir.Location.Var(id) => pure(idToName(node, id, st))
    case ir.Location.Cell(id, index) => for {
      tIndex <- gen(index, st)

      // Bound checking
      size = arraySize(node, id, st)
      _ <- append(Arith.Cmp(size, tIndex))
      _ <- append(Copy.Mov(0L, r10))
      _ <- append(Copy.Mov(1L, r11))
      _ <- append(Copy.Cmovle(r11, r10))
      _ <- append(Arith.Cmp(1L, r10))
      _ <- append(Control.Je(throwOutOfBound.name))

      t <- nextTmp
      _ <- append(Copy.Mov(Location.Array(idToName(node, id, st), tIndex), t)
    } yield t
  }

  def gen(node: ir.MethodCall, st: SymbolCtx): BState[Unit] = for {
    args: List[Location.Name] <- genList(node.args, st)
    _ <- genPushArgs(args, 0)
    _ <- append(Control.Call(idToName(node, node.method, st)))
  } yield ()

  def gen(node: ir.MethodArg, st: SymbolCtx): BState[Location.Name] = node match {
    case ir.MethodArg.ExprArg(e) => gen(e, st)
    case ir.MethodArg.StringArg(s) => // TODO a standard way of postin a string
  }

  def genPushArgs(args: List[Location.Name], i: Int): BState[Unit] =
}

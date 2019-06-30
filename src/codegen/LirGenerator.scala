package codegen

import codegen.State._
import ir._
import lir.Conversion._
import lir.Registers._
import symboltable.{Descriptor, STReadListener, SymbolTable}
import util.State.{get, put, pure}

object LirGenerator /* extends STReadListener[Builder] */ {

  // We could have used (and have tried using) the listener model. However, it is less efficient and not that much
  // more concise than manual recursion

  def initState: Builder = Builder(Nil, Nil, lir.Program(Nil, Nil, Nil), 0, isGlobal = true)

  def idToName(id: ID, st: SymbolTable): String = st.lookup(id.name) match {
    case Some(Descriptor.Array(uid, _, _)) => "ARR_" + id.name + "_" + uid
    case Some(Descriptor.Variable(uid, _)) => "VAR_" + id.name + "_" + uid
    case Some(Descriptor.Method(uid, _)) => if (id.name == "main") id.name else "METHOD_" + id.name + "_" + uid
    case Some(Descriptor.Callout(_)) => id.name
    case _ => throw new RuntimeException
  }

  implicit def gen(node: Program, st: SymbolTable): BState[Unit] = {
    genList(node.fields, st)
  }

  implicit def gen(node: FieldDecl, st: SymbolTable): BState[Unit] = pure()

  def genList[A](nodes: List[A], st: SymbolTable)(implicit gen: (A, SymbolTable) => BState[Unit]): BState[Unit] =
    get >>= (s => put(nodes.foldLeft(s)((s2, n) => gen(n, st)(s2)._2)))

  def enter(node: Ir, st: SymbolTable, s: Builder): Builder = enter(node, st)(s)._2

  def enter(node: Ir, st: SymbolTable): BState[Unit] = node match {
    case _: MethodDecl => toggleIsGlobal
    case _: Block => push(Fragment(Nil, "<block>")) // return name of a block is never used
    case _ => pure()
  }

  def leave(node: Ir, st: SymbolTable, s: Builder): Builder = leave(node, st)(s)._2

  def leave(node: Ir, st: SymbolTable): BState[Unit] = node match {
    case n: VarDecl => leave(n, st)
    case n: ParamDecl => leave(n, st)
    case n: MethodDecl => leave(n, st)
    case n: Expr => leave(n, st)
    case n: Stmt => leave(n, st)
    case n: Location => leave(n, st)
    case _ => pure()
  }

  def leave(node: VarDecl, st: SymbolTable): BState[Unit] = node match {
    case VarDecl.IDDecl(id) => declVar(idToName(id, st))
    case VarDecl.IDArrayDecl(id, size) => declArr(idToName(id, st), size.toLong)
  }

  def leave(node: ParamDecl, st: SymbolTable): BState[Unit] = declVar(node.paramId.name)

  /** Generate code for expression. Each expression 'returns' a simple name */
  def leave(node: Expr, st: SymbolTable): BState[Unit] = node match {
    case n: Expr.UnaryOp => leave(n, st)
    case n: Expr.BinaryOp => leave(n, st)
    case _: Expr.Ternary => for {
      ((f: Fragment, t: Fragment), p: Fragment) <- pop & pop & top
      tmp <- nextTmp
      _ <- extend(t.code)
      _ <- extend(f.code)
      _ <- append(lir.Arith.Cmp(1L, p.ret)) // check if p = 1
      _ <- append(lir.Copy.Mov(t.ret, r10))
      _ <- append(lir.Copy.Mov(f.ret, r11))
      _ <- append(lir.Copy.Cmove(r10, r11))
      _ <- append(lir.Copy.Mov(r11, tmp), tmp)
    } yield ()
    case _: Expr.Load => nextTmp & top >>= (x =>
      append(lir.Copy.Mov(x._2.ret, x._1), x._1))
    case _: Expr.Call => nextTmp >>= (t =>
      append(lir.Copy.Mov(rax, t), t))
    case Expr.Length(id) => nextTmp >>= (t => {
      val len = st.lookup(id.name).get match {
        // TODO check array size
        case Descriptor.Array(_, _, size) => size
        case _ => throw new RuntimeException
      }
      push(Fragment(List(lir.Copy.Mov(len, t)), t))
    })
    case Expr.LitInt(value) => nextTmp >>= (t =>
      push(Fragment(List(lir.Copy.Mov(value.toLong, t)), t)))
    case Expr.LitBool(value) => nextTmp >>= (t =>
      push(Fragment(List(lir.Copy.Mov(if (value) 1L else 0L, t)), t)))
    case Expr.LitChar(value) => nextTmp >>= (t =>
      push(Fragment(List(lir.Copy.Mov(value.toLong, t)), t)))
  }

  def leave(node: Expr.UnaryOp, st: SymbolTable): BState[Unit] = for {
    v <- top
    t <- nextTmp
    _ <- append(lir.Copy.Mov(v.ret, r10))
    r11val = node.op match {
      case Op.Bang() => 1L
      case Op.Minus() => 0L
      case _ => throw new RuntimeException
    }
    _ <- append(lir.Copy.Mov(r11val, r11))
    _ <- append(lir.Arith.Sub(r10, r11))
    _ <- append(lir.Copy.Mov(r11, t), t)
  } yield ()

  def leave(node: Expr.BinaryOp, st: SymbolTable): BState[Unit] =
    (pop & top) >>| (_.swap) >>= (args => node.op match {
      case Op.And() => leaveAndOr(args._1, args._2, isAnd = true)
      case Op.Or() => leaveAndOr(args._1, args._2, isAnd = false)
      case Op.Eqq() | Op.Neq() | Op.Lt() | Op.Gt() | Op.Lte() | Op.Gte() => leaveCmp(args._1, args._2, node.op)
      case Op.Plus() | Op.Minus() | Op.Times() => leavePlusMinusTimes(args._1, args._2, node.op)
      case Op.Div() | Op.Mod() => leaveDiv(args._1, args._2, node.op)
      case _ => throw new RuntimeException
    })

  /** Generate code for and/or binary operation. Assumes that arg1 code is already on the stack */
  def leaveAndOr(arg1: Fragment, arg2: Fragment, isAnd: Boolean): BState[Unit] = for {
    lEnd <- nextCount >>| ((if (isAnd) "AND_" else "OR_") + _ + "_end")
    t <- nextTmp
    _ <- append(lir.Copy.Mov(arg1.ret, r10))
    _ <- append(lir.Arith.Cmp(if (isAnd) 0L else 1L, r10))
    _ <- append(lir.Control.Je(lEnd))
    _ <- extend(arg2.code)
    _ <- append(lir.Copy.Mov(arg2.ret, r10))
    _ <- append(lir.Label(lEnd))
    _ <- append(lir.Copy.Mov(r10, t), t)
  } yield ()

  /** Generate code for comparisons. Assumes that arg1 code is already on the stack */
  def leaveCmp(arg1: Fragment, arg2: Fragment, op: Op): BState[Unit] = for {
    t <- nextTmp
    _ <- extend(arg2.code)
    _ <- append(lir.Copy.Mov(arg1.ret, r10))
    _ <- append(lir.Copy.Mov(arg2.ret, r11))
    _ <- append(lir.Arith.Cmp(r10, r11))
    _ <- append(lir.Copy.Mov(0L, r10))
    _ <- append(lir.Copy.Mov(1L, r11))
    _ <- append(op match {
      case Op.Eqq() => lir.Copy.Cmove(r11, r10)
      case Op.Neq() => lir.Copy.Cmovne(r11, r10)
      case Op.Lt() => lir.Copy.Cmovl(r11, r10)
      case Op.Gt() => lir.Copy.Cmovg(r11, r10)
      case Op.Lte() => lir.Copy.Cmovle(r11, r10)
      case Op.Gte() => lir.Copy.Cmovge(r11, r10)
      case _ => throw new RuntimeException
    })
    _ <- append(lir.Copy.Mov(r10, t), t)
  } yield ()

  /** Generate code for plus, minus, and times. Assumes that arg1 code is already on the stack */
  def leavePlusMinusTimes(arg1: Fragment, arg2: Fragment, op: Op): BState[Unit] = for {
    t <- nextTmp
    _ <- extend(arg2.code)
    _ <- append(lir.Copy.Mov(arg1.ret, r10))
    _ <- append(lir.Copy.Mov(arg2.ret, r11))
    _ <- append(op match {
      case Op.Plus() => lir.Arith.Add(r11, r10)
      case Op.Minus() => lir.Arith.Sub(r11, r10)
      case Op.Times() => lir.Arith.Imul(r11, r10)
      case _ => throw new RuntimeException
    })
    _ <- append(lir.Copy.Mov(r10, t), t)
  } yield ()

  /** Generate code for divide and mod. Assumes that arg1 code is already on the stack */
  def leaveDiv(arg1: Fragment, arg2: Fragment, op: Op): BState[Unit] = for {
    t <- nextTmp
    _ <- extend(arg2.code)
    _ <- append(lir.Copy.Mov(0L, rdx))
    _ <- append(lir.Copy.Mov(arg1.ret, rax))
    _ <- append(lir.Copy.Mov(arg2.ret, r10))
    _ <- append(lir.Arith.Idiv(r10))
    ret = op match {
      case Op.Div() => rax
      case Op.Mod() => rdx
      case _ => throw new RuntimeException
    }
    _ <- append(lir.Copy.Mov(ret, t), t)
  } yield ()

  def leave(node: Stmt, st: SymbolTable): BState[Unit] = node match {
    case _: Stmt.Assign | _: Stmt.PlusAssign | _: Stmt.MinusAssign => leaveAssign(node)
    //    case Stmt.Cond(p, _, _) =>
    //    case Stmt.For(_, start, stop, _, _) =>
    //    case Stmt.While(cond, _) =>
    //    case Stmt.Return(None) => getMethodType(t) match {
    //    case Stmt.Return(_) => getMethodType(t) match {
    //    case _ => Result.Good(t)
  }

  def leaveAssign(node: Stmt): BState[Unit] = for {
    (loc: Fragment, e: Fragment) <- pop & pop
    _ <- extend(loc.code)
    _ <- extend(e.code)
    _ <- append(lir.Copy.Mov(loc.ret, r10))
    _ <- append(lir.Copy.Mov(e.ret, r11))
    _ <- node match {
      case n: Stmt.Assign => append(lir.Copy.Mov(r11, r10))
      case n: Stmt.PlusAssign => append(lir.Arith.Add(r11, r10))
      case n: Stmt.MinusAssign => append(lir.Arith.Sub(r11, r10))
    }
    _ <- append(lir.Copy.Mov(r10, loc.ret))
  } yield ()

  def leave(node: Location, st: SymbolTable): BState[Unit] = node match {
    case Location.Var(id) => push(Fragment(Nil, idToName(id, st)))
    case Location.Cell(id, _) => for {
      f <- top
      t <- nextTmp
      _ <- append(lir.Copy.Mov(lir.Location.Array(idToName(id, st), f.ret.name), t), t)
    } yield ()
  }
}

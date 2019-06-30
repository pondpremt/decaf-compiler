package codegen

import codegen.LirState._
import ir._
import lir.Conversion._
import symboltable.{Descriptor, STReadListener, SymbolTable}
import util.State.get

object LirGenerator extends STReadListener[Builder] {

  def initState: Builder = Builder(Nil, Nil, lir.Program(Nil, Nil, Nil), 0, isGlobal = true)

  def idToName(id: ID, st: SymbolTable): String = st.lookup(id.name) match {
    case Some(Descriptor.Array(uid, _, _)) => "ARR_" + id.name + "_" + uid
    case Some(Descriptor.Variable(uid, _)) => "VAR_" + id.name + "_" + uid
    case Some(Descriptor.Method(uid, _)) => if (id.name == "main") id.name else "METHOD_" + id.name + "_" + uid
    case Some(Descriptor.Callout(_)) => id.name
    case _ => throw new RuntimeException
  }

  def enter(node: Ir, st: SymbolTable, s: Builder): Builder = s

  def leave(node: Ir, st: SymbolTable, s: Builder): Builder = leave(node, st)(s)._1

  def leave(node: Ir, st: SymbolTable): State[Builder] = node match {
    case n: VarDecl => leave(n, st)
    case n: ParamDecl => leave(n, st)
    case n: MethodDecl => leave(n, st)
    case n: Expr => leave(n, st)
    case n: Stmt => leave(n, st)
    case n: Location => leave(n, st)
    case _ => get
  }

  def leave(node: VarDecl, st: SymbolTable): State[Builder] = node match {
    case VarDecl.IDDecl(id) => declVar(idToName(id, st))
    case VarDecl.IDArrayDecl(id, size) => declArr(idToName(id, st), size.toLong)
  }

  def leave(node: ParamDecl, st: SymbolTable): State[Builder] = declVar(node.paramId.name)

  def leave(node: MethodDecl, st: SymbolTable): State[Builder] = toggleIsGlobal

  /** Generate code for expression. Each expression 'returns' a simple name */
  def leave(node: Expr, st: SymbolTable): State[Builder] = node match {
    case n: Expr.UnaryOp => leave(n, st)
    case n: Expr.BinaryOp => leave(n, st)
    case _: Expr.Ternary => for {
      ((f: Fragment, t: Fragment), p: Fragment) <- pop & pop & top
      tmp <- nextTmp
      _ <- extend(t.code)
      _ <- extend(f.code)
      _ <- append(lir.Arith.Cmp(1L, p.ret.get)) // check if p = 1
      _ <- append(lir.Copy.Mov(t.ret.get, lir.Registers.r10))
      _ <- append(lir.Copy.Mov(f.ret.get, lir.Registers.r11))
      _ <- append(lir.Copy.Cmove(lir.Registers.r10, lir.Registers.r11))
      s <- append(lir.Copy.Mov(lir.Registers.r11, tmp), Some(tmp))
    } yield s
    case _: Expr.Load => nextTmp & top >>= (x =>
      append(lir.Copy.Mov(x._2.ret.get, x._1), Some(x._1)))
    case _: Expr.Call => nextTmp >>= (t =>
      append(lir.Copy.Mov(lir.Registers.rax, t), Some(t)))
    case Expr.Length(id) => nextTmp >>= (t => {
      val len = st.lookup(id.name).get match {
        // TODO check array size
        case Descriptor.Array(_, _, size) => size
        case _ => throw new RuntimeException
      }
      push(Fragment(List(lir.Copy.Mov(len, t)), Some(t)))
    })
    case Expr.LitInt(value) => nextTmp >>= (t =>
      push(Fragment(List(lir.Copy.Mov(value.toLong, t)), Some(t))))
    case Expr.LitBool(value) => nextTmp >>= (t =>
      push(Fragment(List(lir.Copy.Mov(if (value) 1L else 0L, t)), Some(t))))
    case Expr.LitChar(value) => nextTmp >>= (t =>
      push(Fragment(List(lir.Copy.Mov(value.toLong, t)), Some(t))))
  }

  def leave(node: Expr.UnaryOp, st: SymbolTable): State[Builder] = for {
    v <- top
    t <- nextTmp
    _ <- append(lir.Copy.Mov(v.ret.get, lir.Registers.r10))
    r11 = node.op match {
      case Op.Bang() => 1L
      case Op.Minus() => 0L
      case _ => throw new RuntimeException
    }
    _ <- append(lir.Copy.Mov(r11, lir.Registers.r11))
    _ <- append(lir.Arith.Sub(lir.Registers.r10, lir.Registers.r11))
    s <- append(lir.Copy.Mov(lir.Registers.r11, t), Some(t))
  } yield s

  /** Generate code for and/or binary operation. Assumes that arg1 code is already on the stack */
  def leaveAndOr(arg1: Fragment, arg2: Fragment, isAnd: Boolean): State[Builder] = for {
    lEnd <- nextCount >>| ((if (isAnd) "AND_" else "OR_") + _ + "_end")
    t <- nextTmp
    _ <- append(lir.Copy.Mov(arg1.ret.get, lir.Registers.r10))
    _ <- append(lir.Arith.Cmp(if (isAnd) 0L else 1L, lir.Registers.r10))
    _ <- append(lir.Control.Je(lEnd))
    _ <- extend(arg2.code)
    _ <- append(lir.Copy.Mov(arg2.ret.get, lir.Registers.r10))
    _ <- append(lir.Label(lEnd))
    s <- append(lir.Copy.Mov(lir.Registers.r10, t), Some(t))
  } yield s

  /** Generate code for comparisons. Assumes that arg1 code is already on the stack */
  def leaveCmp(arg1: Fragment, arg2: Fragment, op: Op): State[Builder] = for {
    t <- nextTmp
    _ <- extend(arg2.code)
    _ <- append(lir.Copy.Mov(arg1.ret.get, lir.Registers.r10))
    _ <- append(lir.Copy.Mov(arg2.ret.get, lir.Registers.r11))
    _ <- append(lir.Arith.Cmp(lir.Registers.r10, lir.Registers.r11))
    _ <- append(lir.Copy.Mov(0L, lir.Registers.r10))
    _ <- append(lir.Copy.Mov(1L, lir.Registers.r11))
    _ <- append(op match {
      case Op.Eqq() => lir.Copy.Cmove(lir.Registers.r11, lir.Registers.r10)
      case Op.Neq() => lir.Copy.Cmovne(lir.Registers.r11, lir.Registers.r10)
      case Op.Lt() => lir.Copy.Cmovl(lir.Registers.r11, lir.Registers.r10)
      case Op.Gt() => lir.Copy.Cmovg(lir.Registers.r11, lir.Registers.r10)
      case Op.Lte() => lir.Copy.Cmovle(lir.Registers.r11, lir.Registers.r10)
      case Op.Gte() => lir.Copy.Cmovge(lir.Registers.r11, lir.Registers.r10)
      case _ => throw new RuntimeException
    })
    s <- append(lir.Copy.Mov(lir.Registers.r10, t), Some(t))
  } yield s

  /** Generate code for plus, minus, and times. Assumes that arg1 code is already on the stack */
  def leavePlusMinusTimes(arg1: Fragment, arg2: Fragment, op: Op): State[Builder] = for {
    t <- nextTmp
    _ <- extend(arg2.code)
    _ <- append(lir.Copy.Mov(arg1.ret.get, lir.Registers.r10))
    _ <- append(lir.Copy.Mov(arg2.ret.get, lir.Registers.r11))
    _ <- append(op match {
      case Op.Plus() => lir.Arith.Add(lir.Registers.r11, lir.Registers.r10)
      case Op.Minus() => lir.Arith.Sub(lir.Registers.r11, lir.Registers.r10)
      case Op.Times() => lir.Arith.Imul(lir.Registers.r11, lir.Registers.r10)
      case _ => throw new RuntimeException
    })
    s <- append(lir.Copy.Mov(lir.Registers.r10, t), Some(t))
  } yield s

  /** Generate code for divide and mod. Assumes that arg1 code is already on the stack */
  def leaveDiv(arg1: Fragment, arg2: Fragment, op: Op): State[Builder] = for {
    t <- nextTmp
    _ <- extend(arg2.code)
    _ <- append(lir.Copy.Mov(0L, lir.Registers.rdx))
    _ <- append(lir.Copy.Mov(arg1.ret.get, lir.Registers.rax))
    _ <- append(lir.Copy.Mov(arg2.ret.get, lir.Registers.r10))
    _ <- append(lir.Arith.Idiv(lir.Registers.r10))
    ret = op match {
      case Op.Div() => lir.Registers.rax
      case Op.Mod() => lir.Registers.rdx
      case _ => throw new RuntimeException
    }
    s <- append(lir.Copy.Mov(ret, t), Some(t))
  } yield s

  def leave(node: Expr.BinaryOp, st: SymbolTable): State[Builder] =
    (pop & top) >>| (_.swap) >>= (args => node.op match {
      case Op.And() => leaveAndOr(args._1, args._2, isAnd = true)
      case Op.Or() => leaveAndOr(args._1, args._2, isAnd = false)
      case Op.Eqq() | Op.Neq() | Op.Lt() | Op.Gt() | Op.Lte() | Op.Gte() => leaveCmp(args._1, args._2, node.op)
      case Op.Plus() | Op.Minus() | Op.Times() => leavePlusMinusTimes(args._1, args._2, node.op)
      case Op.Div() | Op.Mod() => leaveDiv(args._1, args._2, node.op)
      case _ => throw new RuntimeException
    })

  def leave(node: Stmt, st: SymbolTable): State[Builder] = node match {
    case _ => throw new RuntimeException
    ///   case Stmt.Assign(_, _) =>
    //    case Stmt.PlusAssign(loc, e) =>
    //    case Stmt.MinusAssign(loc, e) =>
    //    case Stmt.Cond(p, _, _) =>
    //    case Stmt.For(_, start, stop, _, _) =>
    //    case Stmt.While(cond, _) =>
    //    case Stmt.Return(None) => getMethodType(t) match {
    //    case Stmt.Return(_) => getMethodType(t) match {
    //    case _ => Result.Good(t)
  }

  def leave(node: Location, st: SymbolTable): State[Builder] = node match {
    case Location.Var(id) => push(Fragment(Nil, Some(idToName(id, st))))
    case Location.Cell(id, _) => for {
      f <- top
      t <- nextTmp
      s <- append(lir.Copy.Mov(lir.Location.Array(idToName(id, st), f.ret.get.name), t), Some(t))
    } yield s
  }
}

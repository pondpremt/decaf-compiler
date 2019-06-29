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

  def leave(node: Ir, st: SymbolTable, s: Builder): Builder = s

  def enter(node: Ir, st: SymbolTable, s: Builder): Builder = enter(node, st)(s)._1

  def enter(node: Ir, st: SymbolTable): State[Builder] = node match {
    case n: VarDecl => enter(n, st)
    case n: ParamDecl => enter(n, st)
    case n: MethodDecl => enter(n, st)
    case n: Expr => enter(n, st)
    case n: Stmt => enter(n, st)
    case n: Location => enter(n, st)
    case _ => get
  }

  def enter(node: VarDecl, st: SymbolTable): State[Builder] = node match {
    case VarDecl.IDDecl(id) => declVar(idToName(id, st))
    case VarDecl.IDArrayDecl(id, size) => declArr(idToName(id, st), size.toLong)
  }

  def enter(node: ParamDecl, st: SymbolTable): State[Builder] = declVar(node.paramId.name)

  def enter(node: MethodDecl, st: SymbolTable): State[Builder] = toggleIsGlobal

  // Make sure that ret of every expression is just a name
  def enter(node: Expr, st: SymbolTable): State[Builder] = node match {
    case n: Expr.UnaryOp => enter(n, st)
    case n: Expr.BinaryOp => enter(n, st)
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

  def enter(node: Expr.UnaryOp, st: SymbolTable): State[Builder] = for {
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

  def enter(node: Expr.BinaryOp, st: SymbolTable): State[Builder] =
    (pop & pop) >>| (_.swap) >>= (args => node.op match {
      case _ => throw new RuntimeException
    })

  def enter(node: Stmt, st: SymbolTable): State[Builder] = node match {
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

  def enter(node: Location, st: SymbolTable): State[Builder] = node match {
    case Location.Var(id) => push(Fragment(Nil, Some(idToName(id, st))))
    case Location.Cell(id, _) => for {
      f <- top
      t <- nextTmp
      s <- append(lir.Copy.Mov(lir.Location.Array(idToName(id, st), f.ret.get.name), t), Some(t))
    } yield s
  }
}

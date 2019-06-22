package ir

abstract class SemanticChecker[T] extends STReadListener[(List[SemanticError], T)] {

  final override def initState: (List[SemanticError], T) = (Nil, initCheckerState)

  def initCheckerState: T

  def enter(node: Ir, st: SymbolTable, t: (List[SemanticError], T)): (List[SemanticError], T) =
    enter(node, st, t._2) match {
      case Result.Good(_t) => (t._1, _t)
      case Result.Error(e, _t) => (e :: t._1, _t)
    }

  def leave(node: Ir, st: SymbolTable, t: (List[SemanticError], T)): (List[SemanticError], T) =
    leave(node, st, t._2) match {
      case Result.Good(_t) => (t._1, _t)
      case Result.Error(e, _t) => (e :: t._1, _t)
    }

  def enter(node: Ir, st: SymbolTable, t: T): Result[T]

  def leave(node: Ir, st: SymbolTable, t: T): Result[T]

  sealed abstract class Result[A]

  object Result {

    final case class Good[A](t: A) extends Result[A]

    final case class Error[A](e: SemanticError, t: A) extends Result[A]

  }

}

object TypeChecker extends SemanticChecker[(List[Option[Type]], VoidableType)] {

  // Checked: [5], [6], [7 string literals], 8, 9, 11(b), [13], 14, 15, 16, 17, 18, [19], [20]
  // Overlapping: check that arrays aren't used outside of an expression

  import util.Util

  // (Stack of types, Return type of current method)
  type T = (List[Option[Type]], VoidableType)

  def initCheckerState: T = (Nil, VoidableType.VoidT)

  def enter(node: Ir, st: SymbolTable, t: T): Result[T] = node match {
    case MethodDecl(typ, _, _, _) => Result.Good(setMethodType(t, typ.typ))
    case _ => Result.Good(t)
  }

  def setMethodType(t: T, typ: VoidableType): T = (t._1, typ)

  def leave(node: Ir, st: SymbolTable, t: T): Result[T] = node match {
    case n: Expr => leave(n, st, t)
    case n: Stmt => leave(n, st, t)
    case n: Location => leave(n, st, t)
    case _ => Result.Good(t)
  }

  def leave(node: Stmt, st: SymbolTable, t: T): Result[T] = node match {
    case Stmt.Return(None) => getMethodType(t) match {
      case VoidableType.VoidT => Result.Good(t)
      case _ => Result.Error(SemanticError(node.getSource, "The expression in a return statement must have the same type as the declared result type of\nthe enclosing method definition."), t)
    }
    case Stmt.Return(_) => getMethodType(t) match {
      case VoidableType.VoidT => Result.Error(SemanticError(node.getSource, "A return statement must not have a return value unless it appears in the body of a method\nthat is declared to return a value."), t)
      case VoidableType.Primitive(typ) =>
        if (isNotSame(Some(typ), nth(t, 0))) Result.Error(SemanticError(node.getSource, "The expression in a return statement must have the same type as the declared result type of\nthe enclosing method definition."), pop(t))
        else Result.Good(pop(t))
    }
    case _ => Result.Good(t)
  }

  def pop(t: T, n: Int = 1): T = (t._1.drop(n), t._2)

  def getMethodType(t: T): VoidableType = t._2

  def isNotSame(t1: Option[Type], t2: Option[Type]): Boolean = (t1, t2) match {
    case (Some(typ1), Some(typ2)) => !typ1.equals(typ2)
    case _ => false
  }

  def leave(node: Location, st: SymbolTable, t: T): Result[T] = node match {
    case Location.Var(id) => Result.Good(push(t, st.lookup(id.name).flatMap({
      case Descriptor.Variable(typ) => Some(typ)
      case _ => None
    })))
    case Location.Cell(id, e) =>
      val exprType = st.lookup(id.name).flatMap({
        case Descriptor.Array(typ, _) => Some(typ)
        case _ => None
      })
      if (isNot(PrimitiveType.IntT, nth(t, 0))) Result.Error(SemanticError(e.getSource, "For all locations of the form id[expr], the type of expr must be int; found " + nth(t, 0).get.toString), replace(t, 1, exprType))
      else Result.Good(replace(t, 1, exprType))
  }

  def nth(t: T, n: Int): Option[Type] = t._1(n)

  // Functions to manipulate the state
  def replace(t: T, n: Int, typ: Option[Type]): T = (typ :: t._1.drop(n), t._2)

  def push(t: T, typ: Option[Type]): T = (typ :: t._1, t._2)

  def isNot(expected: Type, actual: Option[Type]): Boolean = actual match {
    case Some(`expected`) | None => false
    case _ => true
  }

  def leave(node: Expr, st: SymbolTable, t: T): Result[T] = node match {
    case n: Expr.Ternary => leave(n, st, t)
    case n: Expr.UnaryOp => leave(n, st, t)
    case n: Expr.BinaryOp => leave(n, st, t)
    case Expr.Load(_) => Result.Good(t)
    case Expr.Call(call) => Result.Good(push(t, st.lookup(call.method.name).flatMap({
      case Descriptor.Method(typ) => Some(typ.value)
      case _ => None
    })))
    case Expr.Length(_) => Result.Good(push(t, Some(PrimitiveType.IntT)))
    case Expr.LitInt(_) => Result.Good(push(t, Some(PrimitiveType.IntT)))
    case Expr.LitBool(_) => Result.Good(push(t, Some(PrimitiveType.BoolT)))
    case Expr.LitChar(_) => Result.Good(push(t, Some(PrimitiveType.CharT)))
  }

  def leave(node: Expr.Ternary, st: SymbolTable, t: T): Result[T] = nth(t, 2) match {
    case None | Some(PrimitiveType.BoolT) =>
      if (isNotSame(nth(t, 1), nth(t, 0))) Result.Error(SemanticError(node.getSource, "The other two expressions in a ternary conditional expression must have the same type; found " + nth(t, 1).get.toString + " and " + nth(t, 0).get.toString), replace(t, 3, None))
      else Result.Good(replace(t, 3, nth(t, 1)))
    case Some(tp) =>
      val te = if (isNotSame(nth(t, 1), nth(t, 0))) None else nth(t, 1)
      Result.Error(SemanticError(node.p.getSource, "The first expr in a ternary conditional expression (?:) must have type boolean; found " + tp.toString), replace(t, 3, te))
  }

  def leave(node: Expr.BinaryOp, st: SymbolTable, t: T): Result[T] = node.op match {
    case Op.Plus() | Op.Minus() | Op.Times() | Op.Div() | Op.Mod() =>
      if (isNot(PrimitiveType.IntT, nth(t, 1))) Result.Error(SemanticError(node.arg1.getSource, "The operands of " + Util.irToString(node.op) + " must have type int; found " + nth(t, 1).get.toString), replace(t, 2, Some(PrimitiveType.IntT)))
      else if (isNot(PrimitiveType.IntT, nth(t, 0))) Result.Error(SemanticError(node.arg2.getSource, "The operands of " + Util.irToString(node.op) + " must have type int; found " + nth(t, 0).get.toString), replace(t, 2, Some(PrimitiveType.IntT)))
      else Result.Good(replace(t, 2, Some(PrimitiveType.IntT)))
    case Op.Lt() | Op.Gt() | Op.Lte() | Op.Gte() =>
      if (isNot(PrimitiveType.IntT, nth(t, 1))) Result.Error(SemanticError(node.arg1.getSource, "The operands of " + Util.irToString(node.op) + " must have type int; found " + nth(t, 1).get.toString), replace(t, 2, Some(PrimitiveType.BoolT)))
      else if (isNot(PrimitiveType.IntT, nth(t, 0))) Result.Error(SemanticError(node.arg2.getSource, "The operands of " + Util.irToString(node.op) + " must have type int; found " + nth(t, 0).get.toString), replace(t, 2, Some(PrimitiveType.BoolT)))
      else Result.Good(replace(t, 2, Some(PrimitiveType.BoolT)))
    case Op.Eqq() | Op.Neq() =>
      if (isNotSame(nth(t, 1), nth(t, 0))) Result.Error(SemanticError(node.getSource, "The operands of " + Util.irToString(node.op) + " must have the same type, either int or boolean; found " + nth(t, 1).get.toString + " and " + nth(t, 0).get.toString), replace(t, 2, Some(PrimitiveType.BoolT)))
      else if (isNot(PrimitiveType.IntT, nth(t, 1)) && isNot(PrimitiveType.BoolT, nth(t, 1))) Result.Error(SemanticError(node.arg1.getSource, "The operands of " + Util.irToString(node.op) + " must have the same type, either int or boolean; found " + nth(t, 1).get.toString + " and " + nth(t, 0).get.toString), replace(t, 2, Some(PrimitiveType.BoolT)))
      else if (isNot(PrimitiveType.IntT, nth(t, 0)) && isNot(PrimitiveType.BoolT, nth(t, 0))) Result.Error(SemanticError(node.arg2.getSource, "The operands of " + Util.irToString(node.op) + " must have the same type, either int or boolean; found " + nth(t, 1).get.toString + " and " + nth(t, 0).get.toString), replace(t, 2, Some(PrimitiveType.BoolT)))
      else Result.Good(replace(t, 2, Some(PrimitiveType.BoolT)))
    case _ =>
      if (isNot(PrimitiveType.BoolT, nth(t, 1))) Result.Error(SemanticError(node.arg1.getSource, "The operands of " + Util.irToString(node.op) + " must have type bool; found " + nth(t, 1).get.toString), replace(t, 2, Some(PrimitiveType.BoolT)))
      else if (isNot(PrimitiveType.BoolT, nth(t, 0))) Result.Error(SemanticError(node.arg2.getSource, "The operands of " + Util.irToString(node.op) + " must have type int; found " + nth(t, 0).get.toString), replace(t, 2, Some(PrimitiveType.IntT)))
      else Result.Good(replace(t, 2, Some(PrimitiveType.BoolT)))
  }

  def leave(node: Expr.UnaryOp, st: SymbolTable, t: T): Result[T] = node.op match {
    case Op.Minus() => nth(t, 0) match {
      case Some(PrimitiveType.IntT) => Result.Good(t)
      case Some(typ) => Result.Error(SemanticError(node.arg.getSource, "UnaryOp " + Util.irToString(node.op) + " expects expression of type int but found " + typ.toString), replace(t, 1, Some(PrimitiveType.IntT)))
      case None => Result.Good(replace(t, 1, Some(PrimitiveType.IntT)))
    }
    case Op.Bang() => nth(t, 0) match {
      case Some(PrimitiveType.BoolT) => Result.Good(t)
      case Some(typ) => Result.Error(SemanticError(node.arg.getSource, "UnaryOp " + Util.irToString(node.op) + " expects expression of type bool but found " + typ.toString), replace(t, 1, Some(PrimitiveType.BoolT)))
      case None => Result.Good(replace(t, 1, Some(PrimitiveType.BoolT)))
    }
    case _ => throw new RuntimeException()
  }


}



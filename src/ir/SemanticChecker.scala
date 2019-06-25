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

  // Checked: 5, 7, 8, 9, 11(b), 13, 14, 15, 16, 17, 18, 19, 20, 21

  import util.Util

  // type T = (Stack of types, Return type of current method)
  // Each Expr and Location node should push net-total of exactly 1 element -- its own type -- to the stack between its
  // entry and exit. No other types should push to the stack. Nodes wih Expr/Location arguments need to pop the
  // corresponding elements upon exit.
  type T = (List[Option[Type]], VoidableType)

  def initCheckerState: T = (Nil, VoidableType.VoidT)

  // Functions to manipulate the state
  def replace(t: T, n: Int, typ: Option[Type]): T = (typ :: t._1.drop(n), t._2)

  def push(t: T, typ: Option[Type]): T = (typ :: t._1, t._2)

  def pop(t: T, n: Int = 1): T = (t._1.drop(n), t._2)

  def nth(t: T, n: Int): Option[Type] = t._1(n)

  def setMethodType(t: T, typ: VoidableType): T = (t._1, typ)

  def getMethodType(t: T): VoidableType = t._2

  def isNotSame(t1: Option[Type], t2: Option[Type]): Boolean = (t1, t2) match {
    case (Some(typ1), Some(typ2)) => typ1.toString != typ2.toString
    case _ => false
  }

  def isNot(expected: Type, actual: Option[Type]): Boolean = actual match {
    case Some(typ) => expected.toString != typ.toString
    case None => false
    case _ => true
  }

  def enter(node: Ir, st: SymbolTable, t: T): Result[T] = node match {
    case MethodDecl(typ, _, _, _) => Result.Good(setMethodType(t, typ.typ))
    case _ => Result.Good(t)
  }

  def leave(node: Ir, st: SymbolTable, t: T): Result[T] = node match {
    case n: Expr => leave(n, st, t)
    case n: Stmt => leave(n, st, t)
    case n: Location => leave(n, st, t)
    case n: MethodCall => leave(n, st, t)
    case _ => Result.Good(t)
  }

  def leave(node: Stmt, st: SymbolTable, t: T): Result[T] = node match {
    case Stmt.Assign(_, _) =>
      if (isNotSame(nth(t, 1), nth(t, 0)))
        Result.Error(SemanticError(node.getSource, "The location and the expr in an assignment must have the same type; found " + nth(t, 1).get.toString + " = " + nth(t, 0).get.toString), pop(t, 2))
      else
        Result.Good(pop(t, 2))
    case Stmt.PlusAssign(loc, e) =>
      if (isNot(PrimitiveType.IntT, nth(t, 1)))
        Result.Error(SemanticError(loc.getSource, "The location in an increment assignment must be of type int; found " + nth(t, 1).get.toString), pop(t, 2))
      else if (isNot(PrimitiveType.IntT, nth(t, 0)))
        Result.Error(SemanticError(e.getSource, "The expr in an increment assignment must be of type int; found " + nth(t, 0).get.toString), pop(t, 2))
      else
        Result.Good(pop(t, 2))
    case Stmt.MinusAssign(loc, e) =>
      if (isNot(PrimitiveType.IntT, nth(t, 1)))
        Result.Error(SemanticError(loc.getSource, "The location in an decrement assignment must be of type int; found " + nth(t, 1).get.toString), pop(t, 2))
      else if (isNot(PrimitiveType.IntT, nth(t, 0)))
        Result.Error(SemanticError(e.getSource, "The expr in an decrement assignment must be of type int; found " + nth(t, 0).get.toString), pop(t, 2))
      else
        Result.Good(pop(t, 2))
    case Stmt.Cond(p, _, _) =>
      if (isNot(PrimitiveType.BoolT, nth(t, 0)))
        Result.Error(SemanticError(p.getSource, "The expr in an if statement must have type boolean; found " + nth(t, 0).get.toString), pop(t))
      else
        Result.Good(pop(t))
    case Stmt.For(_, start, stop, _, _) =>
      if (isNot(PrimitiveType.IntT, nth(t, 1)))
        Result.Error(SemanticError(start.getSource, "The initial expr of for must have type int; found " + nth(t, 1).get.toString), pop(t, 2))
      else if (isNot(PrimitiveType.IntT, nth(t, 0)))
        Result.Error(SemanticError(stop.getSource, "The ending expr of for must have type int; found " + nth(t, 0).get.toString), pop(t, 2))
      else
        Result.Good(pop(t, 2))
    case Stmt.While(cond, _) =>
      if (isNot(PrimitiveType.BoolT, nth(t, 0)))
        Result.Error(SemanticError(cond.getSource, "The expr in n while statement must have type boolean; found " + nth(t, 0).get.toString), pop(t))
      else
        Result.Good(pop(t))
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

  def leave(node: Location, st: SymbolTable, t: T): Result[T] = node match {
    case Location.Var(id) => Result.Good(push(t, st.lookup(id.name).flatMap({
      case Descriptor.Variable(typ) => Some(typ)
      case Descriptor.Array(typ, _) => Some(ArrayType(typ))
      case Descriptor.Callout => Some(CalloutT)
      case Descriptor.Method(typ) => Some(typ)
    })))
    case Location.Cell(id, e) =>
      val exprType = st.lookup(id.name).flatMap({
        case Descriptor.Array(typ, _) => Some(typ)
        case _ => None
      })
      if (isNot(PrimitiveType.IntT, nth(t, 0)))
        Result.Error(SemanticError(e.getSource, "For all locations of the form id[expr], the type of expr must be int; found " + nth(t, 0).get.toString), replace(t, 1, exprType))
      else
        Result.Good(replace(t, 1, exprType))
  }

  def leave(node: Expr, st: SymbolTable, t: T): Result[T] = node match {
    case n: Expr.Ternary => leave(n, st, t)
    case n: Expr.UnaryOp => leave(n, st, t)
    case n: Expr.BinaryOp => leave(n, st, t)
    case Expr.Load(_) => Result.Good(t)
    case Expr.Call(call) => Result.Good(push(t, st.lookup(call.method.name).flatMap({
      case Descriptor.Method(FunctionType(_, typ)) => Some(typ)
      case _ => None
    })))
    case Expr.Length(id) => st.lookup(id.name) match {
      case Some(Descriptor.Array(_, _)) | None => Result.Good(push(t, Some(PrimitiveType.IntT)))
      case _ => Result.Error(SemanticError(id.getSource, "The argument of the @ operator must be an array variable"), push(t, Some(PrimitiveType.IntT)))
    }
    case Expr.LitInt(_) => Result.Good(push(t, Some(PrimitiveType.IntT)))
    case Expr.LitBool(_) => Result.Good(push(t, Some(PrimitiveType.BoolT)))
    case Expr.LitChar(_) => Result.Good(push(t, Some(PrimitiveType.CharT)))
  }

  def leave(node: Expr.Ternary, st: SymbolTable, t: T): Result[T] = nth(t, 2) match {
    case None | Some(PrimitiveType.BoolT) =>
      if (isNotSame(nth(t, 1), nth(t, 0)))
        Result.Error(SemanticError(node.getSource, "The other two expressions in a ternary conditional expression must have the same type; found " + nth(t, 1).get.toString + " and " + nth(t, 0).get.toString), replace(t, 3, None))
      else
        Result.Good(replace(t, 3, nth(t, 1)))
    case Some(tp) =>
      val te = if (isNotSame(nth(t, 1), nth(t, 0))) None else nth(t, 1)
      Result.Error(SemanticError(node.p.getSource, "The first expr in a ternary conditional expression (?:) must have type boolean; found " + tp.toString), replace(t, 3, te))
  }

  def leave(node: Expr.BinaryOp, st: SymbolTable, t: T): Result[T] = node.op match {
    case Op.Plus() | Op.Minus() | Op.Times() | Op.Div() | Op.Mod() =>
      if (isNot(PrimitiveType.IntT, nth(t, 1)))
        Result.Error(SemanticError(node.arg1.getSource, "The operands of " + Util.irToString(node.op) + " must have type int; found " + nth(t, 1).get.toString), replace(t, 2, Some(PrimitiveType.IntT)))
      else if (isNot(PrimitiveType.IntT, nth(t, 0)))
        Result.Error(SemanticError(node.arg2.getSource, "The operands of " + Util.irToString(node.op) + " must have type int; found " + nth(t, 0).get.toString), replace(t, 2, Some(PrimitiveType.IntT)))
      else
        Result.Good(replace(t, 2, Some(PrimitiveType.IntT)))
    case Op.Lt() | Op.Gt() | Op.Lte() | Op.Gte() =>
      if (isNot(PrimitiveType.IntT, nth(t, 1)))
        Result.Error(SemanticError(node.arg1.getSource, "The operands of " + Util.irToString(node.op) + " must have type int; found " + nth(t, 1).get.toString), replace(t, 2, Some(PrimitiveType.BoolT)))
      else if (isNot(PrimitiveType.IntT, nth(t, 0)))
        Result.Error(SemanticError(node.arg2.getSource, "The operands of " + Util.irToString(node.op) + " must have type int; found " + nth(t, 0).get.toString), replace(t, 2, Some(PrimitiveType.BoolT)))
      else
        Result.Good(replace(t, 2, Some(PrimitiveType.BoolT)))
    case Op.Eqq() | Op.Neq() =>
      if (isNotSame(nth(t, 1), nth(t, 0)))
        Result.Error(SemanticError(node.getSource, "The operands of " + Util.irToString(node.op) + " must have the same type, either int or boolean; found " + nth(t, 1).get.toString + " and " + nth(t, 0).get.toString), replace(t, 2, Some(PrimitiveType.BoolT)))
      else if (isNot(PrimitiveType.IntT, nth(t, 1)) && isNot(PrimitiveType.BoolT, nth(t, 1)))
        Result.Error(SemanticError(node.arg1.getSource, "The operands of " + Util.irToString(node.op) + " must have the same type, either int or boolean; found " + nth(t, 1).get.toString + " and " + nth(t, 0).get.toString), replace(t, 2, Some(PrimitiveType.BoolT)))
      else if (isNot(PrimitiveType.IntT, nth(t, 0)) && isNot(PrimitiveType.BoolT, nth(t, 0)))
        Result.Error(SemanticError(node.arg2.getSource, "The operands of " + Util.irToString(node.op) + " must have the same type, either int or boolean; found " + nth(t, 1).get.toString + " and " + nth(t, 0).get.toString), replace(t, 2, Some(PrimitiveType.BoolT)))
      else
        Result.Good(replace(t, 2, Some(PrimitiveType.BoolT)))
    case _ =>
      if (isNot(PrimitiveType.BoolT, nth(t, 1)))
        Result.Error(SemanticError(node.arg1.getSource, "The operands of " + Util.irToString(node.op) + " must have type bool; found " + nth(t, 1).get.toString), replace(t, 2, Some(PrimitiveType.BoolT)))
      else if (isNot(PrimitiveType.BoolT, nth(t, 0)))
        Result.Error(SemanticError(node.arg2.getSource, "The operands of " + Util.irToString(node.op) + " must have type int; found " + nth(t, 0).get.toString), replace(t, 2, Some(PrimitiveType.IntT)))
      else
        Result.Good(replace(t, 2, Some(PrimitiveType.BoolT)))
  }

  def leave(node: Expr.UnaryOp, st: SymbolTable, t: T): Result[T] = node.op match {
    case Op.Minus() =>
      if (isNot(PrimitiveType.IntT, nth(t, 0))) Result.Error(SemanticError(node.arg.getSource, "UnaryOp " + Util.irToString(node.op) + " expects expression of type int but found " + nth(t, 0).get.toString), replace(t, 1, Some(PrimitiveType.IntT)))
      else Result.Good(replace(t, 1, Some(PrimitiveType.IntT)))
    case Op.Bang() =>
      if (isNot(PrimitiveType.BoolT, nth(t, 0))) Result.Error(SemanticError(node.arg.getSource, "UnaryOp " + Util.irToString(node.op) + " expects expression of type bool but found " + nth(t, 0).get.toString), replace(t, 1, Some(PrimitiveType.BoolT)))
      else Result.Good(replace(t, 1, Some(PrimitiveType.BoolT)))
    case _ => throw new RuntimeException()
  }

  // We don't check the contraint about string argument (Constrait 7) here, but we need to make sure there are the same
  // number of stack pushes as there are arguments, so push None for string arguments.
  def leave(node: MethodArg, st: SymbolTable, t: T): Result[T] = node match {
    case MethodArg.ExprArg(_) => Result.Good(t)
    case MethodArg.StringArg(_) => Result.Good(push(t, Some(StringT)))
  }

  def cleanupParams(node: MethodCall, t: T): T = node.args.foldLeft(t)((t: T, arg: MethodArg) => arg match {
    case MethodArg.ExprArg(_) => pop(t)
    case _ => t
  })

  def checkParams(node: MethodCall, paramTyps: List[PrimitiveType], i: Int, t: T): Result[T] =
    if (i == paramTyps.length)
      Result.Good(cleanupParams(node, t))
    else if (isNotSame(Some(paramTyps(i)), nth(t, paramTyps.length - i - 1)))
      Result.Error(SemanticError(node.args(i).getSource, "The type of argument " + (i + 1) + " (" + nth(t, paramTyps.length - i - 1).get + ") must be the same as the type of the formal (" + paramTyps(i) + ")"), cleanupParams(node, t))
    else
      checkParams(node, paramTyps, i + 1, t)

  def leave(node: MethodCall, st: SymbolTable, t: T): Result[T] = st.lookup(node.method.name) match {
    case None | Some(Descriptor.Callout) => Result.Good(cleanupParams(node, t))
    case Some(Descriptor.Method(FunctionType(paramTyps, _))) =>
      if (paramTyps.length != node.args.length)
        Result.Error(SemanticError(node.getSource, "The number of arguments in a method call (" + node.args.length + ") must be the same as the number of the formals (" + paramTyps.length + ")"), cleanupParams(node, t))
      else
        checkParams(node, paramTyps, 0, t)
    case _ => throw new RuntimeException()
  }

}

object IDChecker extends SemanticChecker[Unit] {

  // Checked: 2, 3, 10, 11.a, [12]

  type T = Unit

  def initCheckerState: T = Unit

  def checkDeclared(st: SymbolTable, t: T, id: ID): Result[T] = st.lookup(id.name) match {
    case Some(_) => Result.Good(t)
    case _ => Result.Error(SemanticError(id.getSource, "Identifier " + id.name + " is used before it is declared"), t)
  }

  def enter(node: Ir, st: SymbolTable, t: T): Result[T] = node match {
    case Location.Var(id) => checkDeclared(st, t, id)
    case Expr.Length(id) => checkDeclared(st, t, id)
    case MethodCall(id, _) => checkDeclared(st, t, id)
    case Stmt.For(id, _, _, _, _) => st.lookup(id.name) match {
      case Some(Descriptor.Variable(PrimitiveType.IntT)) => Result.Good(t)
      case _ => Result.Error(SemanticError(id.getSource, "For loop index must have been declared an integer variable"), t)
    }
    case Location.Cell(id, _) => st.lookup(id.name) match {
      case Some(Descriptor.Array(_, _)) => Result.Good(t)
      case Some(_) => Result.Error(SemanticError(id.getSource, "For all locations of the form id[expr], id must be an array variable"), t)
      case _ => Result.Error(SemanticError(id.getSource, "Identifier " + id.name + " is used before it is declared"), t)
    }
    case _ => Result.Good(t)
  }

  def leave(node: Ir, st: SymbolTable, t: T): Result[T] = node match {
    case _: Program => st.lookup("main") match {
      case Some(Descriptor.Method(FunctionType(Nil, VoidableType.VoidT))) => Result.Good(t)
      case _ => Result.Error(SemanticError(node.getSource, "The program must contain a definition for void main () that has no parameters"), t)
    }
    case _ => Result.Good(t)
  }

}

object LitIntChecker extends SemanticChecker[Unit] {

  // checked: 4, 22

  type T = Unit

  def initCheckerState: T = Unit

  def checkBound(node: Ir, value: BigInt, t: T): Result[T] =
    if (value < -BigInt("9223372036854775808")) Result.Error(SemanticError(node.getSource, "The integer is too small: " + value.toString()), t)
    else if (value > BigInt("9223372036854775807")) Result.Error(SemanticError(node.getSource, "The integer is too large: " + value.toString()), t)
    else Result.Good(t)

  def enter(node: Ir, st: SymbolTable, t: T): Result[T] = node match {
    case VarDecl.IDArrayDecl(_, size) =>
      if (size <= 0) Result.Error(SemanticError(node.getSource, "The size in an array declaration must be a positive integer; found " + size), t)
      else checkBound(node, size, t)
    case Stmt.For(_, _, _, step, _) =>
      if (step <= 0) Result.Error(SemanticError(node.getSource, "The step size of a for loop must be a positive integer; found " + step), t)
      else checkBound(node, step, t)
    case Expr.LitInt(value) => checkBound(node, value, t)
    case _ => Result.Good(t)
  }

  def leave(node: Ir, st: SymbolTable, t: T): Result[T] = Result.Good(t)

}

object BreakContChecker extends SemanticChecker[List[Boolean]] {

  //Checked: 23

  type T = List[Boolean]

  def initCheckerState: T = List(false)

  def enter(node: Ir, st: SymbolTable, t: T): Result[T] = node match {
    case _: Stmt.For | _: Stmt.While => Result.Good(true :: t)
    case Stmt.Break() | Stmt.Continue() =>
      if (t.head) Result.Good(t)
      else Result.Error(SemanticError(node.getSource, "All break and continue statements must be contained within the body of a for or a while"), t)
    case _ => Result.Good(t)
  }

  def leave(node: Ir, st: SymbolTable, t: T): Result[T] = node match {
    case _: Stmt.For | _: Stmt.While => Result.Good(t.tail)
    case _ => Result.Good(t)
  }

}

object MustReturnChecker extends SemanticChecker[List[Boolean]] {

  // Checked: 6

  // Each statement pushes true if it definitely returns and false otherwise.
  // Each block pushes true if it definitely returns and false otherwise.
  type T = List[Boolean]

  def initCheckerState: T = Nil

  def enter(node: Ir, st: SymbolTable, t: T): Result[T] = Result.Good(t)

  def leave(node: Ir, st: SymbolTable, t: T): Result[T] = node match {
    case Stmt.Return(_) => Result.Good(true :: t)
    case Stmt.Cond(_, _, None) => Result.Good(false :: t.tail)
    case Stmt.Cond(_, _, Some(_)) => Result.Good((t.head && t(1)) :: t.drop(2))
    case Stmt.For(_, _, _, _, _) => Result.Good(false :: t.tail)
    case Stmt.While(_, _) => Result.Good(false :: t.tail)
    case _: Stmt => Result.Good(false :: t)
    case Block(_, stmts) => Result.Good(t.take(stmts.length).foldLeft(false)(_ || _) :: t.drop(stmts.length))
    case MethodDecl(IrVoidableType(typ), _, _, _) => typ match {
      case VoidableType.VoidT => Result.Good(t.tail)
      case _ =>
        if (t.head) Result.Good(t.tail)
        else Result.Error(SemanticError(node.getSource, "All non-void methods must return a result"), t.tail)
    }
    case _ => Result.Good(t)
  }

}

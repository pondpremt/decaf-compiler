package ir

final case class Walk[S](l: Listener[S]) {

  // Main entry point/dispatcher
  def walkIr: Ir => S = {
    case n: Program => walk(n)(l.init)
    case n: CalloutDecl => walk(n)(l.init)
    case n: FieldDecl => walk(n)(l.init)
    case n: MethodDecl => walk(n)(l.init)
    case n: ID => walk(n)(l.init)
    case n: IrType => walk(n)(l.init)
    case n: IrVoidableType => walk(n)(l.init)
    case n: VarDecl => walk(n)(l.init)
    case n: ParamDecl => walk(n)(l.init)
    case n: Block => walk(n)(l.init)
    case n: Stmt => walk(n)(l.init)
    case n: Location => walk(n)(l.init)
    case n: Expr => walk(n)(l.init)
    case n: MethodCall => walk(n)(l.init)
    case n: MethodArg => walk(n)(l.init)
    case n: Op => walk(n)(l.init)
  }


  implicit def walk(node: Program): S => S =
    wrap(node)(walkList(node.callouts) andThen walkList(node.fields) andThen walkList(node.methods))

  implicit def walk(node: CalloutDecl): S => S = wrap(node)(walk(node.id))

  implicit def walk(node: FieldDecl): S => S =
    wrap(node)(walk(node.typ) andThen walkList(node.ids))

  implicit def walk(node: MethodDecl): S => S =
    wrap(node)(walk(node.typ) andThen walk(node.id) andThen walkList(node.params) andThen walk(node.body))

  implicit def walk(node: ID): S => S = walkLeaf(node)

  implicit def walk(node: IrType): S => S = walkLeaf(node)

  implicit def walk(node: IrVoidableType): S => S = walkLeaf(node)

  implicit def walk(node: VarDecl): S => S = node match {
    case n: VarDecl.IDDecl => wrap(n)(walk(n.id))
    case n: VarDecl.IDArrayDecl => wrap(n)(walk(n.id))
  }

  implicit def walk(node: ParamDecl): S => S = wrap(node)(walk(node.paramType) andThen walk(node.paramId))

  implicit def walk(node: Block): S => S = wrap(node)(walkList(node.fields) andThen walkList(node.stmts))

  implicit def walk(node: Stmt): S => S = wrap(node)(node match {
    case Stmt.Assign(loc, e) => walk(loc) andThen walk(e)
    case Stmt.PlusAssign(loc, e) => walk(loc) andThen walk(e)
    case Stmt.MinusAssign(loc, e) => walk(loc) andThen walk(e)
    case Stmt.CallStmt(c) => walk(c)
    case Stmt.Cond(p, t, f) => walk(p) andThen walk(t) andThen walkOption(f)
    case Stmt.For(i, start, end, _, body) => walk(i) andThen walk(start) andThen walk(end) andThen walk(body)
    case Stmt.While(cond, body) => walk(cond) andThen walk(body)
    case Stmt.Return(value) => walkOption(value)
    case Stmt.Break() => identity[S]
    case Stmt.Continue() => identity[S]
  })

  implicit def walk(node: Location): S => S = wrap(node)(node match {
    case Location.Var(id) => walk(id)
    case Location.Cell(id, index) => walk(id) andThen walk(index)
  })

  implicit def walk(node: Expr): S => S = wrap(node)(node match {
    case Expr.Ternary(p, t, f) => walk(p) andThen walk(t) andThen walk(f)
    case Expr.BinaryOp(op, arg1, arg2) => walk(op) andThen walk(arg1) andThen walk(arg2)
    case Expr.UnaryOp(op, arg) => walk(op) andThen walk(arg)
    case Expr.Load(loc) => walk(loc)
    case Expr.Call(call) => walk(call)
    case Expr.Length(id) => walk(id)
    case Expr.LitInt(_) => identity[S]
    case Expr.LitBool(_) => identity[S]
    case Expr.LitChar(_) => identity[S]
  })

  implicit def walk(node: MethodCall): S => S = wrap(node)(walk(node.method) andThen walkList(node.args))

  implicit def walk(node: MethodArg): S => S = wrap(node)(node match {
    case MethodArg.ExprArg(e) => walk(e)
    case MethodArg.StringArg(_) => identity[S]
  })

  implicit def walk(node: Op): S => S = walkLeaf(node)

  def walkList[A](ls: List[A])(implicit walk: A => S => S): S => S = s => ls.foldLeft(s) { (s, node) => walk(node)(s) }

  def walkOption[A](ls: Option[A])(implicit walk: A => S => S): S => S = ls match {
    case None => identity[S]
    case Some(a) => walk(a)
  }

  private def wrap[A <: Ir](node: A)(f: S => S): S => S = s => l.leave(node, f(l.enter(node, s)))

  private def walkLeaf[A <: Ir](node: A): S => S = s => l.leave(node, l.enter(node, s))

}

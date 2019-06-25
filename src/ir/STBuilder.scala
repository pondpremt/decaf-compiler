package ir

object STBuilder extends STListener[(List[SemanticError], List[SymbolTable])] {

  // Checked: Constrant 1

  def initState: (List[SemanticError], List[SymbolTable]) = (Nil, Nil)

  def put(node: Ir, name: String, desc: Descriptor, s: S): S = s._1.lookupLocal(name) match {
    case Some(_) =>
      (s._1, (SemanticError(node.getSource, "Duplicate declaration of identifier " + name) :: s._2._1, s._2._2))
    case _ =>
      (s._1.put(name, desc), s._2)
  }

  def make(scope: STScope, s: S): S = (SymbolTable.make(s._1, scope), s._2)

  override def enter(node: Ir, s: S): S = node match {
    case CalloutDecl(id) => put(node, id.name, Descriptor.Callout, s)
    case FieldDecl(typ, fields) =>
      fields.foldLeft(s) {
        case (_s, VarDecl.IDDecl(id)) => put(node, id.name, Descriptor.Variable(typ.typ), _s)
        case (_s, VarDecl.IDArrayDecl(id, size)) => put(node, id.name, Descriptor.Array(typ.typ, size.toLong), _s)
      }
    case MethodDecl(typ, id, params, _) =>
      val typs = params.map(_.paramType.typ)
      make(STScope.Params, put(node, id.name, Descriptor.Method(FunctionType(typs, typ.typ)), s))
    case ParamDecl(typ, id) => put(node, id.name, Descriptor.Variable(typ.typ), s)
    case Block(_, _) => make(STScope.Block, s)
    case _ => s
  }

  override def leave(node: Ir, s: S): S = node match {
    case Block(_, _) | MethodDecl(_, _, _, _) | Program(_, _, _) =>
      (s._1.parent.getOrElse(s._1), (s._2._1, s._1 :: s._2._2))
    case _ => s
  }
}

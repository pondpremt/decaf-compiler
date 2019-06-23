package ir

object STBuilder extends STListener[List[SymbolTable]] {

  def initState: List[SymbolTable] = Nil

  override def enter(node: Ir, s: S): S = node match {
    case CalloutDecl(id) => (s._1.put(id.name, Descriptor.Callout), s._2)
    case FieldDecl(typ, fields) =>
      val newSt = fields.foldLeft(s._1) {
        case (st, VarDecl.IDDecl(id)) => st.put(id.name, Descriptor.Variable(typ.typ))
        case (st, VarDecl.IDArrayDecl(id, size)) => st.put(id.name, Descriptor.Array(typ.typ, size))
      }
      (newSt, s._2)
    case MethodDecl(typ, id, params, _) =>
      val typs = params.map(_.paramType.typ)
      val st = s._1.put(id.name, Descriptor.Method(FunctionType(typs, typ.typ)))
      (SymbolTable.make(st), s._2)
    case ParamDecl(typ, id) => (s._1.put(id.name, Descriptor.Variable(typ.typ)), s._2)
    case Block(_, _) => (SymbolTable.make(s._1), s._2)
    case _ => s
  }

  override def leave(node: Ir, s: S): S = node match {
    case Block(_, _) => (s._1.parent.get, s._1 :: s._2)
    case MethodDecl(_, _, _, _) => (s._1.parent.get, s._1 :: s._2)
    case Program(_, _, _) => (s._1, s._1 :: s._2)
    case _ => s
  }
}

package symboltable

import ir._
import semanticchecking.SemanticError

final case class STBuilderState(errs: List[SemanticError],
                                sts: List[SymbolTable],
                                ctx: Map[Ir.UID, SymbolTable],
                                varTyp: Option[PrimitiveType])


object STBuilder extends STListener[STBuilderState] {

  // Checked: Constrant 1

  def initState: STBuilderState = STBuilderState(Nil, Nil, Map(), None)

  def put(node: Ir, name: String, desc: Descriptor, s: S): S = s._1.lookupLocal(name) match {
    case Some(_) =>
      (s._1, s._2.copy(errs = SemanticError(node.getSource, "Duplicate declaration of identifier " + name) :: s._2.errs))
    case _ => (s._1.put(name, desc), s._2)
  }

  def make(scope: STScope, s: S): S = (SymbolTable.make(s._1, scope), s._2)

  override def enter(node: Ir, s: S): S = {
    val s2 = _enter(node, s)
    (s2._1, s2._2.copy(ctx = s2._2.ctx + ((node.uid, s2._1))))
  }

  def _enter(node: Ir, s: S): S = node match {
    case CalloutDecl(id) =>
      put(node, id.name, Descriptor.Callout(), s)
    case FieldDecl(typ, _) =>
      (s._1, s._2.copy(varTyp = Some(typ.typ)))
    case VarDecl.IDDecl(id) =>
      put(node, id.name, Descriptor.Variable(s._2.varTyp.get), s)
    case VarDecl.IDArrayDecl(id, size) =>
      put(node, id.name, Descriptor.Array(s._2.varTyp.get, size.toLong), s)
    case MethodDecl(typ, id, params, _) =>
      val typs = params.map(_.paramType.typ)
      make(STScope.Params, put(node, id.name, Descriptor.Method(FunctionType(typs, typ.typ)), s))
    case ParamDecl(typ, id) =>
      put(node, id.name, Descriptor.Variable(typ.typ), s)
    case Block(_, _) =>
      make(STScope.Block, s)
    case _ => s
  }

  override def leave(node: Ir, s: S): S = node match {
    case _: Block | _: MethodDecl | _: Program =>
      (s._1.parent.getOrElse(s._1), s._2.copy(sts = s._1 :: s._2.sts))
    case _ => s
  }
}

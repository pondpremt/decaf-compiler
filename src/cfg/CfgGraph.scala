package cfg


case class CfgGraph(nodes: Map[CfgNode.ID, CfgNode],
                    root: CfgNode.ID,
                    ins: Map[CfgNode.ID, Set[CfgNode.ID]],
                    outs: Map[CfgNode.ID, CfgExit],
                    labels: Map[String, CfgNode.ID]) {

  override def toString: String = blockOrder(root).map(toString).mkString

  def toString(id: CfgNode.ID): String = nodes(id).toString + (outs(id) match {
    case CfgExit.Terminate => "-----\n\n"
    case CfgExit.Jump(target) => s"----> $target\n\n"
    case CfgExit.FallThrough(target) => s"vvvvv $target\n\n"
    case CfgExit.Conditional(op, t, f) => s"$op ----> $t | $f\n\n"
  })

  private def blockOrder(id: CfgNode.ID): List[CfgNode.ID] = _blockOrder(id, Nil).reverse

  private def _blockOrder(id: CfgNode.ID, visited: List[CfgNode.ID]): List[CfgNode.ID] =
    if (visited contains id)
      visited
    else {
      val visited2 = id :: visited
      outs(id) match {
        case CfgExit.Terminate => visited2
        case CfgExit.Jump(target) => _blockOrder(target, visited2)
        case CfgExit.FallThrough(target) => _blockOrder(target, visited2)
        case CfgExit.Conditional(_, t, f) => _blockOrder(f, _blockOrder(t, visited2))
      }
    }
}



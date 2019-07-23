package cfg

import lir._

private case class ConstructorState()

class CfgConstructor {

  var id: CfgNode.ID = 0

  def nextId(): CfgNode.ID = {
    id += 1
    id
  }

  val sink: CfgNode.ID = 0

  def construct(code: List[Stmt]): CfgGraph = {
    val graph = create(code, Map())
    maximize(graph.root, Set(), graph)._2
  }

  def create(code: List[Stmt], labels: Map[String, CfgNode.ID]): CfgGraph = code match {
    case Nil =>
      val node = CfgNode(sink, List())
      CfgGraph(Map(sink -> node), 0, Map(sink -> Set()), Map(sink -> CfgExit.Terminate), labels)
    case n :: ns =>
      val node = CfgNode(nextId(), List(n))
      val newLabels: Map[String, CfgNode.ID] = n match {
        case label: Control.Label => Map(label.name -> node.id)
        case _ => labels
      }
      val graph = create(ns, newLabels)
      addNode(n, node, graph)
  }

  def addNode(stmt: Stmt, node: CfgNode, graph: CfgGraph): CfgGraph = {
    val exit = stmt match {
      case Control.Jmp(target) => CfgExit.Jump(graph.labels.getOrElse(target, sink))
      case Control.Cjmp(op, target) => CfgExit.Conditional(op, graph.labels.getOrElse(target, sink), graph.root)
      case Control.Ret => CfgExit.Terminate
      case _ => CfgExit.FallThrough(graph.root)
    }
    val entry = exit match {
      case CfgExit.Jump(next) => graph.ins + ((next, graph.ins.getOrElse(next, Set()) + node.id))
      case CfgExit.FallThrough(next) => graph.ins + ((next, graph.ins.getOrElse(next, Set()) + node.id))
      case CfgExit.Conditional(_, t, f) => graph.ins +
        ((t, graph.ins.getOrElse(t, Set()) + node.id)) +
        ((f, graph.ins.getOrElse(f, Set()) + node.id))
      case CfgExit.Terminate => graph.ins
    }
    CfgGraph(graph.nodes + ((node.id, node)), node.id, entry + ((node.id, Set())), graph.outs + ((node.id, exit)), graph.labels)
  }

  def maximize(node: CfgNode.ID, visited: Set[CfgNode.ID], graph: CfgGraph): (Set[CfgNode.ID], CfgGraph) =
    if (visited contains node)
      (visited, graph)
    else
      graph.outs(node) match {
        case CfgExit.Terminate => (visited + node, graph)
        case CfgExit.Jump(target) => maximize(target, visited + node, graph)
        case CfgExit.Conditional(_, t, f) =>
          val (visited2, graph2) = maximize(t, visited + node, graph)
          maximize(f, visited2, graph2)
        case CfgExit.FallThrough(next) => tryMerge(node, next, visited, graph)
      }

  def tryMerge(node: CfgNode.ID, next: CfgNode.ID, visited: Set[CfgNode.ID], graph: CfgGraph): (Set[CfgNode.ID], CfgGraph) =
    (graph.ins(next).size, graph.nodes(next).code) match {
      case (_, Control.Label(_) :: _) => maximize(next, visited + node, graph)
      case (1, stmts) =>
        val node2 = CfgNode(node, graph.nodes(node).code ++ stmts)
        val graph2 = graph.copy(
          nodes = graph.nodes - node - next + ((node, node2)),
          ins = graph.ins - node - next + ((node, graph.ins(node) ++ graph.ins(next))),
          outs = graph.outs - node - next + ((node, graph.outs(next)))
        )
        maximize(node, visited, graph2)
      case _ => maximize(next, visited + node, graph)
    }

}

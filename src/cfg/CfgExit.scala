package cfg

import lir.{Stmt, CmpOp}

sealed abstract class CfgExit

object CfgExit {

  case object Terminate extends CfgExit

  case class Jump(node: CfgNode.ID) extends CfgExit

  case class FallThrough(node: CfgNode.ID) extends CfgExit

  case class Conditional(op: CmpOp, t: CfgNode.ID, f: CfgNode.ID) extends CfgExit

}


package cfg

import lir._

case class CfgNode(id: CfgNode.ID, code: List[Stmt]) {

  override def toString: String = s"BasicBlock $id\n" + code.map(codegen.CodeGenerator.gen).mkString

}

object CfgNode {

  type ID = Long

}



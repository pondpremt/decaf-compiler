package util

object Util {

  def irToString(node: ir.Ir): String = ir.Walk(ir.PrettyPrintListener).walkIr(node)._2.head

}

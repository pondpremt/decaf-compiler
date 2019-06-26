package util

object Util {

  def irToString(node: ir.Ir): String = ir.Walk(PrettyPrintListener).walkIr(node)._2.head

}

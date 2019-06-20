package ir

final case class SymbolTable(parent: Option[SymbolTable], tab: Map[String, Descriptor])

object SymbolTable {

  def base: SymbolTable = SymbolTable(None, Map())

}

sealed abstract class Descriptor

object Descriptor {

  final case class Variable(typ: PrimitiveType) extends Descriptor

  final case class Array(typ: PrimitiveType, size: Int) extends Descriptor

  final case class Method(typ: FunctionType) extends Descriptor

  final case object Callout extends Descriptor

}



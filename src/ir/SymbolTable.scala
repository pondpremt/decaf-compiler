package ir

final case class SymbolTable(parent: Option[SymbolTable], tab: Map[String, Descriptor]) {

  def put(s: String, d: Descriptor): SymbolTable = this.copy(tab = tab + ((s, d)))

  def lookup(s: String): Option[Descriptor] = tab.get(s).orElse(parent.flatMap(_.lookup(s)))

  def lookupLocal(s: String): Option[Descriptor] = tab.get(s)

}

object SymbolTable {

  def base: SymbolTable = SymbolTable(None, Map())

  def make(parent: SymbolTable): SymbolTable = SymbolTable(Some(parent), Map())

}

sealed abstract class Descriptor

object Descriptor {

  final case class Variable(typ: PrimitiveType) extends Descriptor

  final case class Array(typ: PrimitiveType, size: Int) extends Descriptor

  final case class Method(typ: FunctionType) extends Descriptor

  final case object Callout extends Descriptor

}



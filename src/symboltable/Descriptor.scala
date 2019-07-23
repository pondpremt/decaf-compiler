package symboltable

import ir.{FunctionType, PrimitiveType}

sealed abstract class Descriptor extends util.WithUID

object Descriptor {

  type UID = util.WithUID.UID

  final case class Variable(typ: PrimitiveType) extends Descriptor

  final case class Array(typ: PrimitiveType, size: Long) extends Descriptor

  final case class Method(typ: FunctionType) extends Descriptor

  final case class Callout() extends Descriptor

}

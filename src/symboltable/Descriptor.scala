package symboltable

import ir.{FunctionType, PrimitiveType}

sealed abstract class Descriptor {
  val uid: Long
}

object Descriptor {

  final case class Variable(uid: Long, typ: PrimitiveType) extends Descriptor

  final case class Array(uid: Long, typ: PrimitiveType, size: Long) extends Descriptor

  final case class Method(uid: Long, typ: FunctionType) extends Descriptor

  final case class Callout(uid: Long) extends Descriptor

}

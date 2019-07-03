package symboltable

import ir.{FunctionType, PrimitiveType}

sealed abstract class Descriptor {

  val uid: Descriptor.UID = Descriptor.newUid()

}

object Descriptor {

  type UID = Long

  private var uid: UID = 0L

  private def newUid(): UID = {
    uid += 1;
    uid
  }

  final case class Variable(typ: PrimitiveType) extends Descriptor

  final case class Array(typ: PrimitiveType, size: Long) extends Descriptor

  final case class Method(typ: FunctionType) extends Descriptor

  final case class Callout() extends Descriptor

}

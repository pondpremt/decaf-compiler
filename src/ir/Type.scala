package ir

sealed abstract class Type

sealed abstract class PrimitiveType extends Type

object PrimitiveType {

  final case object IntT extends PrimitiveType

  final case object BoolT extends PrimitiveType

}

sealed abstract class VoidableType extends Type

object VoidableType {

  final case object VoidT extends VoidableType

  final case class Primitive(typ: PrimitiveType) extends VoidableType

}

final case class FunctionType(params: List[PrimitiveType], value: VoidableType) extends Type

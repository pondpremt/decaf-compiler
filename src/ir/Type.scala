package ir

sealed abstract class Type

sealed abstract class PrimitiveType extends Type

object PrimitiveType {

  final case object IntT extends PrimitiveType {
    override def toString: String = "int"
  }

  final case object BoolT extends PrimitiveType {
    override def toString: String = "bool"
  }

  final case object CharT extends PrimitiveType {
    override def toString: String = "char"
  }

}

sealed abstract class VoidableType extends Type

object VoidableType {

  final case class Primitive(typ: PrimitiveType) extends VoidableType {
    override def toString: String = typ.toString
  }

  final case object VoidT extends VoidableType {
    override def toString: String = "void"
  }

}

final case class FunctionType(params: List[PrimitiveType], value: VoidableType) extends Type {
  override def toString: String = "(" + params.map(_.toString).mkString(", ") + ") => " + value.toString
}

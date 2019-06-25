package ir

sealed abstract class Type {
  // Must be precise enough to be used for type comparison; i.e., two t1.toString == t2.toString iff t1 and t2 represent
  // the same program type
  override def toString: String
}

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

final case class ArrayType(typ: PrimitiveType) extends Type {
  override def toString: String = "[" + typ + "]"
}

case object StringT extends Type {
  override def toString: String = "string"
}

final case class FunctionType(params: List[PrimitiveType], value: VoidableType) extends Type {
  override def toString: String = "function: (" + params.map(_.toString).mkString(", ") + ") => " + value.toString
}

case object CalloutT extends Type {
  override def toString: String = "callout"
}

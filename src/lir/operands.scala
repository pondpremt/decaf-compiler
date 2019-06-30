package lir

sealed abstract class Source

object Source {

  final case class Lit(value: Long) extends Source

  final case class Loc(loc: Location) extends Source

}

sealed abstract class Location

object Location {

  final case class Reg(reg: Register) extends Location

  final case class Addr(offset: Long, reg: Register, reg2: Option[Register]) extends Location

  final case class Name(name: String) extends Location

  final case class Array(name: String, offset: String) extends Location

}

sealed abstract class Target

object Target {

  // final case class Addr(offset: Long, reg: Register) extends Target

  final case class Label(name: String) extends Target

}

final case class Register(name: String)

object Registers {
  val rax: Register = Register("rax")
  val rbx: Register = Register("rbx")
  val rcx: Register = Register("rcx")
  val rdx: Register = Register("rdx")
  val rsp: Register = Register("rsp")
  val rbp: Register = Register("rbp")
  val rsi: Register = Register("rsi")
  val rdi: Register = Register("rdi")
  val r8: Register = Register("r8")
  val r9: Register = Register("r9")
  val r10: Register = Register("r10")
  val r11: Register = Register("r11")
  val r12: Register = Register("r12")
  val r13: Register = Register("r13")
  val r14: Register = Register("r14")
  val r15: Register = Register("r15")
}

object Conversion {

  // implicit def sourceToLocation: Source => Location = {
  //   case Source.Loc(loc) => loc
  //   case _ => throw new RuntimeException("A literal is not a location")
  // }

  implicit def locationToSource: Location => Source = Source.Loc

  implicit def registerToLocation: Register => Location = Location.Reg

  implicit def registerToSource: Register => Source = registerToLocation andThen locationToSource

  implicit def valueToSource: Long => Source = Source.Lit

  implicit def stringToName: String => Location.Name = Location.Name

  implicit def nameToString: Location.Name => String = _.name

  implicit def stringToLocation: String => Location = stringToName

  implicit def stringToSource: String => Source = stringToLocation andThen locationToSource

  implicit def stringToTarget: String => Target = Target.Label


}

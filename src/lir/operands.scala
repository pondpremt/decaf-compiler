package lir

sealed abstract class Source

object Source {

  final case class Lit(value: Long) extends Source

  final case class Loc(loc: Location) extends Source

}

sealed abstract class Location

object Location {

  final case class Reg(reg: Register) extends Location

  final case class Addr(offset: Either[Long, String], reg: Option[Register], reg2: Option[Register], scale: Long) extends Location

  final case class Name(name: String) extends Location

}

final case class Register(name: String)

object Registers {
  val rax: Register = Register("rax")
  val rbx: Register = Register("rbx")
  val rcx: Register = Register("rcx")
  val rdx: Register = Register("rdx")
  val rsp: Register = Register("rsp")
  val rbp: Register = Register("rbp")
  val rip: Register = Register("rip")
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

  implicit def locationToSource: Location => Source = Source.Loc

  implicit def registerToLocation: Register => Location = Location.Reg

  implicit def registerToSource: Register => Source = registerToLocation andThen locationToSource

  implicit def valueToSource: Long => Source = Source.Lit

  implicit def stringToName: String => Location.Name = Location.Name

  implicit def nameToString: Location.Name => String = _.name

  implicit def stringToLocation: String => Location = stringToName

  implicit def stringToSource: String => Source = stringToLocation andThen locationToSource


}

package ir

case class Program(callouts: List[CalloutDecl], fields: List[FieldDecl], methods: List[MethodDecl])

case class CalloutDecl(id: ID)

case class FieldDecl(typ: Type, ids: List[VarDecl])

case class MethodDecl(typ: Option[Type], id: ID, params: List[ParamDecl], body: Block)

case class ParamDecl(paramType: Type, paramId: ID)

case class Block(fields: List[FieldDecl], stmt: List[Stmt])

sealed abstract class Stmt
object Stmt {
  final case class Assign(location: Location, e: Expr) extends Stmt
  final case class PlusAssign(location: Location, e: Expr) extends Stmt
  final case class MinusAssign(location: Location, e: Expr) extends Stmt
  final case class CallStmt(call: Expr.Call) extends Stmt
  final case class Cond(p: Expr, t: Block, f: Option[Block]) extends Stmt
  final case class For(index: ID, start: Expr, stop: Expr, step: Option[Expr], body: Block) extends Stmt
  final case class While(cond: Expr, body: Block) extends Stmt
  final case class Return(value: Option[Expr]) extends Stmt
  final case object Break extends Stmt
  final case object Continue extends Stmt
}

sealed abstract class Location
object Location {
  final case class Var(id: ID) extends Location
  final case class Cell(id: ID, index: Expr) extends Location
}

sealed abstract class Expr
object Expr {
  final case class Ternary(p: Expr, t: Expr, f: Expr) extends Expr
  final case class BinaryOp(op: Op, arg1: Expr, arg2: Expr) extends Expr
  final case class UnaryOp(op: Op, arg: Expr) extends Expr
  final case class Load(loc: Location) extends Expr
  final case class Call(method: ID, args: List[MethodArg]) extends Expr
  final case class Length(id: ID) extends Expr
  final case class LitInt(value: Int) extends Expr
  final case class LitBool(value: Boolean) extends Expr
  final case class LitChar(value: Char) extends Expr
}

sealed abstract class MethodArg
object MethodArg {
  final case class ExprArg(e: Expr) extends MethodArg
  final case class StringArg(s: String) extends MethodArg
}

case class ID(name: String)

sealed abstract class VarDecl
object VarDecl {
  final case class IDDecl(id: ID) extends VarDecl
  final case class IDArrayDecl(id: ID, size: Int) extends VarDecl
}

sealed abstract class Op
object Op {
  final case object And extends Op
  final case object Or extends Op
  final case object Eqq extends Op
  final case object Neq extends Op
  final case object Lt extends Op
  final case object Gt extends Op
  final case object Lte extends Op
  final case object Gte extends Op
  final case object Plus extends Op
  final case object Minus extends Op
  final case object Times extends Op
  final case object Div extends Op
  final case object Mod extends Op
  final case object Bang extends Op
}

sealed abstract class Type
object Type {
  final case object IntT extends Type
  final case object BooleanT extends Type
}


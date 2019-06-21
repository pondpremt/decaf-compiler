package ir

sealed abstract class Ir(implicit src: Source) {

  def getSite: Option[(Int, Int)] = src.getSite

}

case class Program(callouts: List[CalloutDecl],
                   fields: List[FieldDecl],
                   methods: List[MethodDecl])
                  (implicit src: Source) extends Ir

case class CalloutDecl(id: ID)(implicit src: Source) extends Ir

case class FieldDecl(typ: IrType, ids: List[VarDecl])(implicit src: Source) extends Ir

case class MethodDecl(typ: IrVoidableType, id: ID, params: List[ParamDecl], body: Block)(implicit src: Source) extends Ir

case class ParamDecl(paramType: IrType, paramId: ID)(implicit src: Source) extends Ir

case class Block(fields: List[FieldDecl], stmts: List[Stmt])(implicit src: Source) extends Ir

sealed abstract class Stmt(implicit src: Source) extends Ir

object Stmt {

  final case class Assign(location: Location, e: Expr)(implicit src: Source) extends Stmt

  final case class PlusAssign(location: Location, e: Expr)(implicit src: Source) extends Stmt

  final case class MinusAssign(location: Location, e: Expr)(implicit src: Source) extends Stmt

  final case class CallStmt(call: MethodCall)(implicit src: Source) extends Stmt

  final case class Cond(p: Expr, t: Block, f: Option[Block])(implicit src: Source) extends Stmt

  final case class For(index: ID, start: Expr, stop: Expr, step: Int, body: Block)(implicit src: Source)
    extends Stmt

  final case class While(cond: Expr, body: Block)(implicit src: Source) extends Stmt

  final case class Return(value: Option[Expr])(implicit src: Source) extends Stmt

  final case class Break()(implicit src: Source) extends Stmt

  final case class Continue()(implicit src: Source) extends Stmt

}

sealed abstract class Location(implicit src: Source) extends Ir

object Location {

  final case class Var(id: ID)(implicit src: Source) extends Location

  final case class Cell(id: ID, index: Expr)(implicit src: Source) extends Location

}

sealed abstract class Expr(implicit src: Source) extends Ir

object Expr {

  final case class Ternary(p: Expr, t: Expr, f: Expr)(implicit src: Source) extends Expr

  final case class BinaryOp(op: Op, arg1: Expr, arg2: Expr)(implicit src: Source) extends Expr

  final case class UnaryOp(op: Op, arg: Expr)(implicit src: Source) extends Expr

  final case class Load(loc: Location)(implicit src: Source) extends Expr

  final case class Call(call: MethodCall)(implicit src: Source) extends Expr

  final case class Length(id: ID)(implicit src: Source) extends Expr

  final case class LitInt(value: Int)(implicit src: Source) extends Expr

  final case class LitBool(value: Boolean)(implicit src: Source) extends Expr

  final case class LitChar(value: Char)(implicit src: Source) extends Expr

}

case class MethodCall(method: ID, args: List[MethodArg])(implicit src: Source) extends Ir

sealed abstract class MethodArg(implicit src: Source) extends Ir

object MethodArg {

  final case class ExprArg(e: Expr)(implicit src: Source) extends MethodArg

  final case class StringArg(s: String)(implicit src: Source) extends MethodArg

}

case class ID(name: String)(implicit src: Source) extends Ir

sealed abstract class VarDecl(implicit src: Source) extends Ir

object VarDecl {

  final case class IDDecl(id: ID)(implicit src: Source) extends VarDecl

  final case class IDArrayDecl(id: ID, size: Int)(implicit src: Source) extends VarDecl

}

sealed abstract class Op(implicit src: Source) extends Ir

object Op {

  final case class And()(implicit src: Source) extends Op

  final case class Or()(implicit src: Source) extends Op

  final case class Eqq()(implicit src: Source) extends Op

  final case class Neq()(implicit src: Source) extends Op

  final case class Lt()(implicit src: Source) extends Op

  final case class Gt()(implicit src: Source) extends Op

  final case class Lte()(implicit src: Source) extends Op

  final case class Gte()(implicit src: Source) extends Op

  final case class Plus()(implicit src: Source) extends Op

  final case class Minus()(implicit src: Source) extends Op

  final case class Times()(implicit src: Source) extends Op

  final case class Div()(implicit src: Source) extends Op

  final case class Mod()(implicit src: Source) extends Op

  final case class Bang()(implicit src: Source) extends Op

}

final case class IrType(typ: PrimitiveType)(implicit src: Source) extends Ir

final case class IrVoidableType(typ: VoidableType)(implicit src: Source) extends Ir


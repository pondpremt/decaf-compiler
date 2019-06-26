package util

import ir._

object PrettyPrintListener extends Listener[(String, List[String])] {

  override type S = (String, List[String])

  def consume(f: String => String): S => S = {
    case (i, s) => (i, f(s.head) :: s.tail)
  }

  def consume(f: (String, String) => String): S => S = {
    case (i, s) => (i, f(s(1), s.head) :: s.drop(2))
  }

  def consume(f: (String, String, String) => String): S => S = {
    case (i, s) => (i, f(s(2), s(1), s.head) :: s.drop(3))
  }

  def consume(f: (String, String, String, String) => String): S => S = {
    case (i, s) => (i, f(s(3), s(2), s(1), s.head) :: s.drop(4))
  }

  def consume(f: (String, String, String, String, String) => String): S => S = {
    case (i, s) => (i, f(s(4), s(3), s(2), s(1), s.head) :: s.drop(5))
  }

  def consumeN(n: Int, skip: Int = 0)(f: List[String] => String): S => S = {
    case (i, s) => (i, s.take(skip) ::: f(s.slice(skip, skip + n).reverse) :: s.drop(n + skip))
  }

  def indent(f: S => S): S => S = f andThen (s => consume(s._1 + _)(s))

  def push(x: String): S => S = s => (s._1, x :: s._2)

  def init: S = ("", Nil)

  def enter(node: Ir, s: S): S = node match {
    case _: Block => (s._1 + "    ", "{\n" :: s._2)
    case _ => s
  }

  // Re-route to the curried implementation of leave
  def leave(node: Ir, s: S): S = leave(node)(s)

  def leave(node: Ir): S => S = node match {
    case Program(callouts, fields, methods) =>
      consumeN(callouts.length + fields.length + methods.length)(_.mkString(""))
    case CalloutDecl(_) =>
      indent(consume("callout " + _ + ";\n"))
    case FieldDecl(_, ids) =>
      indent(consumeN(ids.length)(_.mkString(", ")) andThen consume(_ + " " + _ + ";\n"))
    case MethodDecl(_, _, params, _) =>
      indent(consumeN(params.length, 1)(_.mkString(", ")) andThen consume(_ + " " + _ + " (" + _ + ") " + _ + "\n"))
    case MethodCall(_, args) =>
      consumeN(args.length)(_.mkString(", ")) andThen consume(_ + "(" + _ + ")")
    case VarDecl.IDDecl(_) => identity[S]
    case VarDecl.IDArrayDecl(_, size) => consume(_ + "[" + size.toString + "]")
    case ParamDecl(_, _) => consume(_ + " " + _)
    case Location.Var(_) => identity[S]
    case Location.Cell(_, _) => consume(_ + "[" + _ + "]")
    case MethodArg.ExprArg(e) => identity[S]
    case MethodArg.StringArg(s) => push(s)
    case ID(name) => push(name)
    case IrType(typ) => push(typ.toString)
    case IrVoidableType(typ) => push(typ.toString)
    case n: Op => push(opToString(n))
    case n: VarDecl => leave(n)
    case n: Block => leave(n)
    case n: Stmt => leave(n)
    case n: Expr => leave(n)
  }

  def leave(node: Block): S => S = ((s: S) => (s._1.dropRight(4), s._2)).andThen(
    indent(push("}")) andThen consumeN(2 + node.fields.length + node.stmts.length)(_.mkString("")))

  def leave(node: Stmt): S => S = consume(_ + "\n") compose indent(node match {
    case Stmt.Assign(_, _) => consume(_ + " = " + _ + ";")
    case Stmt.PlusAssign(_, _) => consume(_ + " += " + _ + ";")
    case Stmt.MinusAssign(_, _) => consume(_ + " += " + _ + ";")
    case Stmt.CallStmt(_) => consume(_ + ";")
    case Stmt.Cond(_, _, None) => consume("if (" + _ + ") " + _)
    case Stmt.Cond(_, _, Some(_)) => consume("if (" + _ + ") " + _ + " else " + _)
    case Stmt.For(_, _, _, step, _) => consume("for (" + _ + " = " + _ + ", " + _ + ", " + step + ") " + _)
    case Stmt.While(_, _) => consume("while (" + _ + ") " + _)
    case Stmt.Return(None) => push("return;")
    case Stmt.Return(Some(_)) => consume("return " + _ + ";")
    case Stmt.Break() => push("break;")
    case Stmt.Continue() => push("continue;")
  })

  def leave(node: Expr): S => S = node match {
    case Expr.Ternary(_, _, _) => consume("(" + _ + " ? " + _ + " : " + _ + ")")
    case Expr.BinaryOp(_, _, _) => consume((op, arg1, arg2) => "(" + arg1 + " " + op + " " + arg2 + ")")
    case Expr.UnaryOp(_, _) => consume(_ + _)
    case Expr.Load(_) => identity[S]
    case Expr.Call(_) => identity[S]
    case Expr.Length(_) => consume("@" + _)
    case Expr.LitInt(value) => push(value.toString)
    case Expr.LitBool(value) => push(value.toString)
    case Expr.LitChar(value) => push(value.toString)
  }

  def opToString: ir.Op => String = {
    case ir.Op.And() => "&&"
    case ir.Op.Or() => "||"
    case ir.Op.Eqq() => "=="
    case ir.Op.Neq() => "!="
    case ir.Op.Lt() => "<"
    case ir.Op.Gt() => ">"
    case ir.Op.Lte() => "<="
    case ir.Op.Gte() => ">="
    case ir.Op.Plus() => "+"
    case ir.Op.Minus() => "-"
    case ir.Op.Times() => "*"
    case ir.Op.Div() => "/"
    case ir.Op.Mod() => "%"
    case ir.Op.Bang() => "!"
  }

}


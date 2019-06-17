package parsing

import edu.mit.compilers.grammar.DecafParserTokenTypes

object TreeParser {
  def parseProgram(implicit node: ParseTree): ir.Program = {
    type Accum = (List[ir.CalloutDecl], List[ir.FieldDecl], List[ir.MethodDecl])
    val program = node.foldl[Accum]((Nil, Nil, Nil)) { (program, node) => node.getType match {
        case DecafParserTokenTypes.TK_callout => program.copy(_1 = parseCallout(node) :: program._1)
        case DecafParserTokenTypes.FIELD_DECL => program.copy(_2 = parseFieldDecl(node) :: program._2)
        case DecafParserTokenTypes.METHOD_DECL => program.copy(_3 = parseMethodDecl(node) :: program._3)
      }
    }
    ir.Program(program._1.reverse, program._2.reverse, program._3.reverse)
  }

  def parseCallout(implicit node: ParseTree): ir.CalloutDecl = ir.CalloutDecl(parseId(node.getChild(0)))

  def parseId(implicit node: ParseTree): ir.ID = ir.ID(node.getText)

  def parseFieldDecl(implicit node: ParseTree): ir.FieldDecl = {
    val typ = parseType(node.getChild(0))
    val ids = node.map(1) { n => parseFieldId(n) }
    ir.FieldDecl(typ, ids)
  }

  def parseType(implicit node: ParseTree): ir.Type = node.getType match {
    case DecafParserTokenTypes.TK_int => ir.Type.IntT()
    case DecafParserTokenTypes.TK_boolean => ir.Type.BooleanT()
  }

  def parseFieldId(implicit node: ParseTree): ir.VarDecl = {
    val id = parseId(node.getChild(0))
    node.getNumberOfChildren match {
      case 1 => ir.VarDecl.IDDecl(id)
      case 2 => ir.VarDecl.IDArrayDecl(id, node.getChild(1).getText.toInt)
    }
  }

  def parseMethodDecl(implicit node: ParseTree): ir.MethodDecl = {
    val typ = node.getChild(0).getType match {
      case DecafParserTokenTypes.TK_void => Option.empty
      case _ => Option.apply(parseType(node.getChild(0)))
    }
    val id = parseId(node.getChild(1))
    val params = node.getChild(2).map(0, 0, 2) { n =>
      ir.ParamDecl(parseType(n), parseId(n.getNextSibling.asInstanceOf[ParseTree]))(n)
    }
    val body = parseBlock(node.getChild(3))
    ir.MethodDecl(typ, id, params, body)
  }

  def parseBlock(implicit node: ParseTree): ir.Block = {
    type Accum = (List[ir.FieldDecl], List[ir.Stmt])
    val block = node.foldl[Accum]((Nil, Nil)) { (block, node) => node.getType match {
      case DecafParserTokenTypes.FIELD_DECL => block.copy(_1 = parseFieldDecl(node) :: block._1)
      case _ => block.copy(_2 = parseStmt(node) :: block._2)
    }}
    ir.Block(block._1.reverse, block._2.reverse)
  }

  def parseStmt(implicit node: ParseTree): ir.Stmt = node.getType match {
    case DecafParserTokenTypes.ASSIGNMENT => parseAssignment
    case DecafParserTokenTypes.METHOD_CALL => ir.Stmt.CallStmt(parseMethodCall)
    case DecafParserTokenTypes.TK_if => parseIf(node)
    case DecafParserTokenTypes.TK_for => parseFor(node)
    case DecafParserTokenTypes.TK_while => parseWhile(node)
    case DecafParserTokenTypes.TK_return => parseReturn(node)
    case DecafParserTokenTypes.TK_break => ir.Stmt.Break()
    case DecafParserTokenTypes.TK_continue => ir.Stmt.Continue()
  }

  def parseAssignment(implicit node: ParseTree): ir.Stmt = {
    val loc = parseLocation(node.getChild(0))
    val e = parseExpr(node.getChild(2))
    node.getChild(1).getType match {
      case DecafParserTokenTypes.EQ      => ir.Stmt.Assign(loc, e)
      case DecafParserTokenTypes.PLUSEQ  => ir.Stmt.PlusAssign(loc, e)
      case DecafParserTokenTypes.MINUSEQ => ir.Stmt.MinusAssign(loc, e)
    }
  }

  def parseLocation(implicit node: ParseTree): ir.Location = {
    val id = parseId(node.getChild(0))
    node.getNumberOfChildren match {
      case 1 => ir.Location.Var(id)
      case 2 => ir.Location.Cell(id, parseExpr(node.getChild(1)))
    }
  }

  def parseExpr(implicit node: ParseTree): ir.Expr = node.getType match {
    case DecafParserTokenTypes.QUES => ir.Expr.Ternary(
      parseExpr(node.getChild(0)), parseExpr(node.getChild(1)), parseExpr(node.getChild(2)))
    case DecafParserTokenTypes.LOCATION => ir.Expr.Load(parseLocation)
    case DecafParserTokenTypes.METHOD_CALL =>ir.Expr.Call(parseMethodCall)
    case DecafParserTokenTypes.INT_LITERAL => ir.Expr.LitInt(node.getText.toInt)
    case DecafParserTokenTypes.CHAR_LITERAL => ir.Expr.LitChar(node.getText.charAt(0))
    case DecafParserTokenTypes.TK_true => ir.Expr.LitBool(value=true)
    case DecafParserTokenTypes.TK_false => ir.Expr.LitBool(value=false)
    case _ => node.getNumberOfChildren match {
      case 1 => ir.Expr.UnaryOp(parseOp, parseExpr(node.getChild(0)))
      case 2 => ir.Expr.BinaryOp(parseOp, parseExpr(node.getChild(0)), parseExpr(node.getChild(1)))
    }
  }

  def parseMethodCall(implicit node: ParseTree): ir.MethodCall = {
    val id = parseId(node.getChild(0))
    val params = node.map[ir.MethodArg](1) { n => parseMethodArg(n) }
    ir.MethodCall(id, params)
  }

  def parseMethodArg(implicit node: ParseTree): ir.MethodArg = node.getType match {
    case DecafParserTokenTypes.STRING_LITERAL => ir.MethodArg.StringArg(node.getText)
    case _ => ir.MethodArg.ExprArg(parseExpr)
  }

  def parseOp(implicit node: ParseTree): ir.Op = node.getType match {
    case DecafParserTokenTypes.AND   => ir.Op.And()
    case DecafParserTokenTypes.OR    => ir.Op.Or()
    case DecafParserTokenTypes.EQQ   => ir.Op.Eqq()
    case DecafParserTokenTypes.NEQ   => ir.Op.Neq()
    case DecafParserTokenTypes.LT    => ir.Op.Lt()
    case DecafParserTokenTypes.GT    => ir.Op.Gt()
    case DecafParserTokenTypes.LTE   => ir.Op.Lte()
    case DecafParserTokenTypes.GTE   => ir.Op.Gte()
    case DecafParserTokenTypes.PLUS  => ir.Op.Plus()
    case DecafParserTokenTypes.MINUS => ir.Op.Minus()
    case DecafParserTokenTypes.TIMES => ir.Op.Times()
    case DecafParserTokenTypes.DIV   => ir.Op.Div()
    case DecafParserTokenTypes.MOD   => ir.Op.Mod()
    case DecafParserTokenTypes.BANG  => ir.Op.Bang()
  }

  def parseIf(implicit node: ParseTree): ir.Stmt.Cond = ir.Stmt.Cond(
    parseExpr(node.getChild(0)),
    parseBlock(node.getChild(1)),
    node.getNumberOfChildren match {
      case 2 => Option.empty
      case 3 => Option.apply(parseBlock(node.getChild(2)))
    })

  def parseFor(implicit node: ParseTree): ir.Stmt.For = {
    val index = parseId(node.getChild(0))
    val start = parseExpr(node.getChild(1))
    val stop = parseExpr(node.getChild(2))
    val (step, body) = node.getNumberOfChildren match {
      case 4 => (1, parseBlock(node.getChild(3)))
      case 5 => (node.getChild(3).getText.toInt, parseBlock(node.getChild(4)))
    }
    ir.Stmt.For(index, start, stop, step, body)
  }

  def parseWhile(implicit node: ParseTree): ir.Stmt.While =
    ir.Stmt.While(parseExpr(node.getChild(0)), parseBlock(node.getChild(1)))

  def parseReturn(implicit node: ParseTree): ir.Stmt.Return = node.getNumberOfChildren match {
    case 0 => ir.Stmt.Return(Option.empty)
    case 1 => ir.Stmt.Return(Option.apply(parseExpr(node.getChild(0))))
  }
}

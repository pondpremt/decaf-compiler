package compile

import antlr.collections.AST
import edu.mit.compilers.grammar.DecafParserTokenTypes

object ASTBuilder {
  def buildProgram(node: AST): ir.Program = {
    def dispatch(program: (List[ir.CalloutDecl], List[ir.FieldDecl], List[ir.MethodDecl]), node: AST) =
      node.getType match {
        case DecafParserTokenTypes.TK_callout => program.copy(_1 = buildCallout(node) :: program._1)
        case DecafParserTokenTypes.FIELD_DECL => program.copy(_2 = buildFieldDecl(node) :: program._2)
        case DecafParserTokenTypes.METHOD_DECL => program.copy(_3 = buildMethodDecl(node) :: program._3)
      }

    val program = foldl(node, dispatch, (Nil, Nil, Nil))
    ir.Program(program._1.reverse, program._2.reverse, program._3.reverse)
  }

  def buildCallout(node: AST): ir.CalloutDecl = ir.CalloutDecl(buildId(node.getFirstChild))

  def buildId(node: AST): ir.ID = ir.ID(node.getText)

  def buildType(node: AST): ir.Type = node.getType match {
    case DecafParserTokenTypes.TK_int => ir.Type.IntT
    case DecafParserTokenTypes.TK_boolean => ir.Type.BooleanT
  }

  def buildFieldDecl(node: AST): ir.FieldDecl = {
    val typ = buildType(node.getFirstChild)
    val ids = map(node, buildFieldId, 1)
    ir.FieldDecl(typ, ids)
  }

  def buildFieldId(node: AST): ir.VarDecl = {
    val c1 = node.getFirstChild
    val id = buildId(c1)
    if (node.getNumberOfChildren == 1)
      ir.VarDecl.IDDecl(id)
    else {
      val size = Integer.parseInt(c1.getNextSibling.getText)
      ir.VarDecl.IDArrayDecl(id, size)
    }
  }

  def buildMethodDecl(node: AST): ir.MethodDecl = {
    val c1 = node.getFirstChild
    val typ = c1.getType match {
      case DecafParserTokenTypes.TK_void => Option.empty
      case _ => Option.apply(buildType(c1))
    }
    val c2 = c1.getNextSibling
    val id = buildId(c2)
    val c3 = c2.getNextSibling
    val params = map(c3, (n: AST) => ir.ParamDecl(buildType(n), buildId(n.getNextSibling)), 0, 0, 2)
    val c4 = c3.getNextSibling
    val body = buildBlock(c4)
    ir.MethodDecl(typ, id, params, body)
  }

  def buildBlock(node: AST): ir.Block = {
    null
  }

  def foldl[A](node: AST, f: (A, AST) => A, init: A, dropHead: Int = 0, dropTail: Int = 0, stride: Int = 1): A = {
    val n = node.getNumberOfChildren

    def foldlHelper(node: AST, accum: A, i: Int): A =
      if (i == n)
        accum
      else {
        val accum_ = if (i < dropHead || (n - i) <= dropTail || (i - dropHead) % stride != 0) accum else f(accum, node)
        foldlHelper(node.getNextSibling, accum_, i + 1)
      }

    foldlHelper(node.getFirstChild, init, 0)
  }

  def map[A](node: AST, f: AST => A, dropHead: Int = 0, dropTail: Int = 0, stride: Int = 1): List[A] =
    foldl(node, (accum: List[A], n: AST) => f(n) :: accum, Nil, dropHead, dropTail, stride).reverse
}

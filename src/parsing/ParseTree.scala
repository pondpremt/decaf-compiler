package parsing

import antlr.collections.AST
import antlr.{CommonAST, Token}
import ir.Source

class ParseTree extends CommonAST with Source {
  private var line = 0
  private var column = 0

  override def initialize(tok: Token) {
    super.initialize(tok)
    line = tok.getLine
    column = tok.getColumn
  }

  override def getLine:   Int = this.getSite.getOrElse((0, 0))._1
  override def getColumn: Int = this.getSite.getOrElse((0, 0))._2

  def getChild(n: Int): ParseTree =
    if (n == 0) this.getFirstChild.asInstanceOf[ParseTree]
    else this.getChild(n - 1).getNextSibling.asInstanceOf[ParseTree]

  def foldl[A](init: A, dropHead: Int = 0, dropTail: Int = 0, stride: Int = 1)(f: (A, ParseTree) => A): A = {
    var accum = init
    var child = this.getFirstChild.asInstanceOf[ParseTree]
    val n = getNumberOfChildren
    for (i <- 0 until this.getNumberOfChildren) {
      if (i >= dropHead && (n - i) > dropTail && (i - dropHead) % stride == 0)
        accum = f(accum, this.getChild(i))
      child = this.getNextSibling.asInstanceOf[ParseTree]
    }
    accum
  }

  def map[A](dropHead: Int = 0, dropTail: Int = 0, stride: Int = 1)(f: ParseTree => A): List[A] =
    foldl(Nil: List[A], dropHead, dropTail, stride) { (accum: List[A], n: ParseTree) => f(n) :: accum }.reverse

  def getSite: Option[(Int, Int)] =
    if (line != 0) Option.apply(line, column)
    else if (this.getNumberOfChildren > 0) this.getChild(0).getSite
    else Option.empty
}

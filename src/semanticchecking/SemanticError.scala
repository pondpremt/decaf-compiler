package semanticchecking

import ir.Source

case class SemanticError(src: Source, msg: String) {

  override def toString: String = src.getSite match {
    case Some((l, c)) => "Semantic Error at (" + l + ", " + c + "): " + msg
    case None => "Semantic Error " + msg
  }

}



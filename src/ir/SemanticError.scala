package ir

case class SemanticError(src: Source, msg: String) {
  override def toString: String = "Semantic Error: " + msg
}



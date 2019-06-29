package symboltable

final case class SymbolTable(parent: Option[SymbolTable], scope: STScope, tab: Map[String, Descriptor]) {

  def put(s: String, d: Descriptor): SymbolTable = this.copy(tab = tab + ((s, d)))

  def lookup(s: String): Option[Descriptor] = tab.get(s).orElse(parent.flatMap(_.lookup(s)))

  def lookupLocal(s: String): Option[Descriptor] = tab.get(s).orElse(parent.map(_.scope) match {
    case Some(STScope.Params) => parent.flatMap(_.lookupLocal(s))
    case _ => None
  })

}

object SymbolTable {

  def base(scope: STScope): SymbolTable = SymbolTable(None, scope, Map())

  def make(parent: SymbolTable, scope: STScope): SymbolTable = SymbolTable(Some(parent), scope, Map())

}

sealed abstract class STScope

object STScope {

  final case object Global extends STScope

  final case object Params extends STScope

  final case object Block extends STScope

}

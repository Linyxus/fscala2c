package fs2c.ast

/** A symbol in the Scala code.
  * 
  * @param name Symbol name.
  * @param dealias Reference contained in the symbol.
  * @tparam T Referenced type.
  */
case class Symbol[T](name: String, var dealias: T) {
  override def toString: String = s"Symbol($name)"
}

object Symbol {
  enum Ref {
    case Unresolved(symName: String)
    case Resolved[T](sym: Symbol[T])
    
    def isResolved: Boolean = this match {
      case _ : Unresolved => false
      case _ => true
    }
    
    def nonResolved: Boolean = !isResolved
    
    def name: String = this match {
      case Resolved(sym) => sym.name
      case Unresolved(symName) => symName
    }
  }
}

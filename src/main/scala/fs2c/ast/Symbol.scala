package fs2c.ast

/** A symbol in the Scala code.
  * 
  * @param name Symbol name.
  * @param dealias Reference contained in the symbol.
  * @tparam T Referenced type.
  */
case class Symbol[T](name: String, var dealias: T)

object Symbol {
  enum Ref {
    case Unresolved(symName: String)
    case Resolved[T](sym: Symbol[T])
    
    def isResolved: Boolean = this match {
      case _ : Unresolved => false
      case _ => true
    }
    
    def nonResolved: Boolean = !isResolved
  }
}

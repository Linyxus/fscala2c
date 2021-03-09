package fs2c.ast

/** Scope is a linked list of mappings from symbol names to symbols.
  */
object Scopes {
  /** Scope for lexical analysis.
    *
    * @param syms   Symbols contained in the scope.
    * @param parent Parent scope of the current one. None if this is the root scope.
    */
  case class ParseScope(var syms: Map[String, Symbol[_]], parent: ParseScope)
  
  class ScopeContext {
    /** Current scope.
      * Initially, it will be an empty root scope.
      */
    var currentScope: ParseScope = ParseScope(Map.empty, null)

    /** Enter a new scope.
      */
    def locateScope(): Unit = {
      currentScope = ParseScope(Map.empty, currentScope)
    }

    /** Exit the current scope.
      */
    def relocateScope(): Unit = {
      currentScope = currentScope.parent.ensuring(_ != null, "can not relocate from the root scope.")
    }

    /** Find symbol in the given scope. Will not search deeper into its parent scope.
      *
      * @param scope   Scope to search in.
      * @param symName Symbol name to look for.
      */
    def findSymIn(scope: ParseScope, symName: String): Option[Symbol[_]] = scope.syms get symName

    /** Find symbol from all nested scopes.
      *
      * @param symName Symbol name to look for.
      */
    def findSym(symName: String): Option[Symbol[_]] = {
      
      @annotation.tailrec def recur(scope: ParseScope): Option[Symbol[_]] = scope match {
        case null => None
        case _ => findSymIn(scope, symName) match {
          case Some(sym) => Some(sym)
          case None => recur(scope.parent)
        }
      }

      recur(currentScope)
    }

    /** Find symbol in the current scope.
      */
    def findSymHere(symName: String): Option[Symbol[_]] = findSymIn(currentScope, symName)

    /** Add a symbol into the current scope.
      */
    def addSymbol(sym: Symbol[_]): Unit = {
      currentScope.syms = currentScope.syms.updated(sym.name, sym)
    }
  }
}

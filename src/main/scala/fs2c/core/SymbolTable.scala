package fs2c.core

import fs2c.ast.Symbol
import collection.mutable

class SymbolTable {
  import SymbolTable._

  var current = TableNode(mutable.Map.empty, null)

  def locate(): Unit = current = TableNode(mutable.Map.empty, current)

  def relocate(): Unit = {
    assert(current.previous ne null, "can not relocate from the root scope")
    current = current.previous
  }

  def addSymbol(sym: Symbol[_]): Unit =
    current.current.update(sym.name, sym)

  private def lookupIn(node: TableNode, name: String): Option[Symbol[_]] =
    node.current.get(name)

  /** Looks up a symbol of `name` in table.
    */
  def lookup(name: String): Option[Symbol[_]] = {
    @annotation.tailrec def recur(node: TableNode): Option[Symbol[_]] = node match {
      case null => None
      case n @ TableNode(_, previous) =>
        lookupIn(n, name) match {
          case None => recur(previous)
          case v => v
        }
    }

    recur(current)
  }

  def lookupCurrent(name: String): Option[Symbol[_]] = lookupIn(current, name)
}

object SymbolTable {
  case class TableNode(current: mutable.Map[String, Symbol[_]], previous: TableNode)
}

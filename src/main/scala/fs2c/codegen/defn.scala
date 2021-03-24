package fs2c.codegen

import fs2c.ast.c.{ Trees => C }
import fs2c.ast.Symbol

object defn {
  /** C type: (void *)
    */
  val VoidPointer = C.PointerType(C.VoidType)

  /** Generate a local variable definition with type `tp` and `name`.
    */
  def localVariable(name: String, tp: C.Type): (Symbol[C.VariableDef], C.Block) = {
    val d: C.VariableDef = name :: tp
    val block = List(C.Statement.Def(d))
    
    (d.sym, block)
  }

  def assignVar(sym: Symbol[C.VariableDef], expr: C.Expr): C.Statement =
    C.Statement.AssignVar(sym, expr)
}

package fs2c.codegen

import fs2c.ast.c.{Trees => C}
import fs2c.ast.Symbol
import fs2c.codegen.{ CodeBundles => bd }

object defn {
  import GroundFuncs._
  /** C type: (void *)
    */
  val VoidPointer = C.PointerType(C.VoidType)

  /** Generate a local variable definition with type `tp` and `name`.
    */
  def localVariable(name: String, tp: C.Type, expr: Option[C.Expr] = None): (Symbol[C.VariableDef], C.Block) = {
    val d: C.VariableDef = C.VariableDef.makeVariableDef(name, tp, expr)
    val block = List(C.Statement.Def(d))
    
    (d.sym, block)
  }

  def assignVar(binding: C.Binding, expr: C.Expr): C.Statement =
    C.Statement.AssignVar(binding, expr)
    
  def assignMember(v: C.Binding, d: Symbol[C.StructMember], expr: C.Expr): C.Statement =
    v.getType match {
      case C.StructType(structSym) =>
        assert(structSym eq d.dealias.struct, s"the designator should have the same struct type with the variable: $structSym and ${d.dealias.struct}")
        C.Statement.AssignMember(v, d, expr)
      case _ =>
        assert(false, "variable should be a struct")
    }
  
  def funcType(paramTypes: List[C.Type], valueType: C.Type): C.FuncType =
    C.FuncType(valueType, paramTypes)
    
  def allocStruct(structDef: C.StructDef): C.Expr = malloc $$ C.SizeOf(structDef.tp)
    
  object GroundFuncs {
    val malloc = C.GroundFunc("malloc", List("stdlib.h"))
    val scanf = C.GroundFunc("scanf", List("stdio.h"))
    val printf = C.GroundFunc("printf", List("stdio.h"))
    val srand = C.GroundFunc("srand", List("stdlib.h"))
    val rand = C.GroundFunc("rand", List("stdlib.h"))
    val time = C.GroundFunc("time", List("time.h"))
  }
}

package fs2c.codegen

import fs2c.ast.Symbol
import fs2c.ast.c.{ Trees => C }
import fs2c.core.SymbolTable

class CodeGenContext {
  /** All generated C definitions.
    */
  var generatedDefs: List[C.Definition] = Nil
  
  /** Cache for all generated type alias for C function type.
    */
  var genFuncCache: Map[C.FuncType, C.TypeAliasDef] = Map.empty

  /** Current scope for generated C definitions.
    */
  val cScope: SymbolTable = new SymbolTable
}

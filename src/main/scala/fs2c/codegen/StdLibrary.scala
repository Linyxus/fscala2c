package fs2c.codegen

import fs2c.ast.c.{Trees => C}

class StdLibrary(val gen: CodeGen) {
  import StdLibrary.LibBundle

  /** Structure for function closure.
    * ```c
    * struct func_closure {
    *   void *func;
    *   void *env;
    * }
    * ```
    */
  val FuncClosure: LibBundle[C.StructDef] = new LibBundle[C.StructDef](gen) {
    override protected def bundle: C.StructDef =
      C.StructDef.makeStructDef(
        name = "func_closure",
        memberDefs = List(
          "func" -> defn.VoidPointer,
          "env" -> defn.VoidPointer
        )
      )
  }

}

object StdLibrary {
  trait LibBundle[T <: C.Definition](gen: CodeGen) {
    private var loaded: Boolean = false
    
    def isLoaded: Boolean = loaded
    
    def load: T = 
      if loaded then
        bundle
      else {
        loaded = true
        dependencies foreach { dep => dep.load }
        val d = bundle
        gen.outDef(d)
        d
      }
    
    protected def bundle: T
    
    protected def dependencies: List[LibBundle[_]] = Nil
  }
}

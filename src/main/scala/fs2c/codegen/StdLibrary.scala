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
  val FuncClosure: LibBundle = new LibBundle(gen) {
    override protected def bundle: C.Definition =
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
  trait LibBundle(gen: CodeGen) {
    private var loaded: Boolean = false
    
    def isLoaded: Boolean = loaded
    
    def load: C.Definition = 
      if loaded then
        bundle
      else {
        loaded = true
        dependencies foreach { dep => dep.load }
        val d = bundle
        gen.outDef(d)
        d
      }
    
    protected def bundle: C.Definition
    
    protected def dependencies: List[LibBundle] = Nil
  }
}

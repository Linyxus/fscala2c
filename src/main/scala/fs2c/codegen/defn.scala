package fs2c.codegen

import fs2c.ast.c.{ Trees => C }

object defn {
  /** C type: (void *)
    */
  val VoidPointer = C.PointerType(C.VoidType)
}

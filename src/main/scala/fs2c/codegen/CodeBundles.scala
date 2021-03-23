package fs2c.codegen

import fs2c.ast.fs.{ Trees => FS }
import fs2c.ast.c.{ Trees => C }

object CodeBundles {
  
  type CodeBundleOf[X] = X match
    case FS.Expr[_] => Unit
    case _ => Unit
  
  case class PureBundle(expr: C.Expr)

}

package fs2c.codegen

import fs2c.ast.fs.{ Trees => FS }
import fs2c.ast.c.{ Trees => C }

object Trees {
  
  import CodeBundles._

  /** Attaches generated code information to trees.
    */
  case class Generated[X](tree: X, var code: CodeBundleOf[X])

  type GenTree = [X[_[_]]] =>> Generated[X[Generated]]
  
  object gen {
    type ClassDef = GenTree[FS.ClassDef]
    type MemberDef = GenTree[FS.MemberDef]

    type Expr = GenTree[FS.Expr]

    type LiteralIntExpr = GenTree[FS.LiteralIntExpr]
    type LiteralFloatExpr = GenTree[FS.LiteralFloatExpr]
    type LiteralBooleanExpr = GenTree[FS.LiteralBooleanExpr]

    type LambdaExpr = GenTree[FS.LambdaExpr]

    type BlockExpr = GenTree[FS.BlockExpr]
    type LocalDef = GenTree[FS.LocalDef]
    type LocalDefBind = GenTree[FS.LocalDef.Bind]
    type LocalDefEval = GenTree[FS.LocalDef.Eval]
    type LocalDefAssign = GenTree[FS.LocalDef.Assign]

    type BinOpExpr = GenTree[FS.BinOpExpr]
    type UnaryOpExpr = GenTree[FS.UnaryOpExpr]

    type ApplyExpr = GenTree[FS.ApplyExpr]
    type SelectExpr = GenTree[FS.SelectExpr]

    type IfExpr = GenTree[FS.IfExpr]

    type NewExpr = GenTree[FS.NewExpr]

    type IdentifierExpr = GenTree[FS.IdentifierExpr]
  }

}

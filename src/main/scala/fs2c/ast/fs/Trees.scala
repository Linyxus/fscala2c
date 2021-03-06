package fs2c.ast.fs

import fs2c.ast.Symbol
import fs2c.typer.Types._

/** Abstract syntax trees for Featherweight Scala
  *
  * The tree contains extra structure introduced by F. For example, for some tree type SomeTree:
  *
  *   - A untyped tree can be expressed as `Untyped[SomeTree[Untyped]]`.
  *   - A typed tree can be expressed as `Typed[SomeTree[Typed]]`.
  */
trait Trees {

  /** The trait for all terms living in the value domain.
    */
  trait Term

  /** A class definition.
    *
    * ```scala
    * class Foo extends Bar {
    * // members
    * }
    * ```
    * would be converted to
    * ```scala
    * ClassDef(sym = Foo, parent = Some(Bar), List(...))
    * ```
    */
  case class ClassDef[F[_]](sym: Symbol[F[ClassDef[F]]], parent: Option[Symbol[F[ClassDef[F]]]], members: List[F[MemberDef[F]]]) extends Type

  /** A member definition in the class body.
    */
  case class MemberDef[F[_]](sym: Symbol[ClassDef[F]], mutable: Boolean, ascript: Option[Type], body: F[Expr[F]]) extends Term

  /** Tree for expressions
    */
  sealed trait Expr[F[_]] extends Term

  /** Lambda expressions.
    */
  case class LambdaExpr[F[_]](params: List[Symbol[LambdaParam]], tpe: Option[Type], body: F[Expr[F]]) extends Expr[F]

  /** Parameter in the lambda expression.
    */
  case class LambdaParam(sym: Symbol[LambdaParam], tpe: Type)

  /** Block expressions.
    */
  case class BlockExpr[F[_]](defs: List[F[LocalDef[F]]], expr: F[Expr[F]]) extends Expr[F]

  /** Identifier that refers to a symbol.
    */
  case class IdentifierExpr[F[_]](sym: Symbol.Ref) extends Expr[F]

  /** Local definitions in block expressions.
    */
  enum LocalDef[F[_]] {
    /** Statement for local bindings.
      * `val` will be parsed into a immutable bind, while `var` will become a mutable one.
      */
    case Bind[F[_]](sym: Symbol[F[Bind[F]]], mutable: Boolean, tpe: Option[Type], body: F[Expr[F]]) extends LocalDef[F]

    /** Statement evaluating an expression in a block.
      * Any expression in the block will be parsed as [[LocalDef.Eval]].
      */
    case Eval[F[_]](expr: F[Expr[F]]) extends LocalDef[F]
  }

  /** Type projectors to embed extra information into the tree.
    * Currently placeholders.
    */
  case class Untyped[+X](tree: X)
  type Typed = [X] =>> X

  /** Transform tree type to Untyped tree type. For more information, see [[Untyped]].
    */
  type UntypedTree = [T[_[_]]] =>> Untyped[T[Untyped]]

  /** Transform tree type to Typed tree type. For more information, see [[Typed]].
    */
  type TypedTree = [T[_[_]]] =>> Typed[T[Typed]]

  /** Untyped trees.
    */
  object untpd {
    type Expr = UntypedTree[Trees.Expr]
    
    type LambdaExpr = UntypedTree[Trees.LambdaExpr]
    
    type BlockExpr = UntypedTree[Trees.BlockExpr]
    type LocalDef = UntypedTree[Trees.LocalDef]
    type LocalDefBind = UntypedTree[Trees.LocalDef.Bind]
    type LocalDefEval = UntypedTree[Trees.LocalDef.Eval]
    
    type IdentifierExpr = UntypedTree[Trees.IdentifierExpr]
  }
}

object Trees extends Trees

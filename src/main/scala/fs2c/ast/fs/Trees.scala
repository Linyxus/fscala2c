package fs2c.ast.fs

import fs2c.ast.Symbol
import fs2c.typer.Types._

/** Abstract syntax trees for Featherweight Scala
 *
 *  The tree contains extra structure introduced by F. For example, for some tree type SomeTree:
 *
 *   - A untyped tree can be expressed as `Untyped[SomeTree[Untyped]]`.
 *   - A typed tree can be expressed as `Typed[SomeTree[Typed]]`.
 */
trait Trees {

  /** A tag for Class values. Defined classes like ClassDef or builtin types.
   */
  trait ClassValue

  /** A class definition.
   *
   *  ```scala
   *  class Foo extends Bar {
   *    // members
   *  }
   *  ```
   *  would be converted to
   *  ```scala
   *  ClassDef(sym = Foo, parent = Some(Bar), List(...))
   *  ```
   */
  case class ClassDef[F[_]](sym: Symbol[ClassDef[F]], parent: Option[Symbol[ClassValue]], members: List[F[MemberDef[F]]]) extends ClassValue

  /** A member definition in the class body.
   */
  case class MemberDef[F[_]](sym: Symbol[ClassDef[F]], mutable: Boolean, ascript: Option[Type], body: F[Expr[F]])

  /** Tree for expressions
   */
  sealed trait Expr[F[_]]

  /** Lambda expressions.
   */
  case class LambdaExpr[F[_]](params: List[Symbol[LambdaParam[F]]], ascript: Option[Type], body: F[Expr[F]]) extends Expr[F]
  /** Parameter in the lambda expression.
   */
  case class LambdaParam[F[_]](sym: Symbol[LambdaParam[F]], ascript: Option[Type], lambda: F[LambdaExpr[F]])

  /** Block expressions.
   */
  case class BlockExpr[F[_]](defs: List[F[LocalDef[F]]], expr: F[Expr[F]]) extends Expr[F]

  /** Local definitions in block expressions.
   */
  enum LocalDef[F[_]] {
    case Val[F[_]](sym: Symbol[Val[F]], ascript: Option[Type], body: F[Expr[F]]) extends LocalDef[F]
    case Var[F[_]](sym: Symbol[Val[F]], ascript: Option[Type], body: F[Expr[F]]) extends LocalDef[F]
    case Eval[F[_]](body: F[Expr[F]]) extends LocalDef[F]
  }

  /** Type projectors to embed extra information into the tree.
   *  Currently placeholders.
   */
  type Untyped = [X] =>> X
  type Typed = [X] =>> X

  /** Transform tree type to Untyped tree type. For more information, see [[Untyped]].
   */
  type UntypedTree = [T[_[_]]] =>> Untyped[T[Untyped]]

  /** Transform tree type to Typed tree type. For more information, see [[Typed]].
   */
  type TypedTree = [T[_[_]]] =>> Typed[T[Typed]]
}

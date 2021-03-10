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
object Trees {

  /** The trait for all terms living in the value domain.
    */
  trait Term

  /** A class definition.
    *
    * ```scala
    * class Foo(a : Int, b : Boolean) extends Bar {
    * // members
    * }
    * ```
    * would be converted to
    * ```scala
    * ClassDef(sym = Foo, params = List(a : Int, b : Boolean), parent = Some(Bar), List(...))
    * ```
    */
  case class ClassDef[F[_]](sym: Symbol[F[ClassDef[F]]], params: List[Symbol[LambdaParam]], parent: Option[Symbol.Ref], members: List[F[MemberDef[F]]]) extends Type

  /** A member definition in the class body.
    */
  case class MemberDef[F[_]](sym: Symbol[F[MemberDef[F]]], var classDef: Symbol[F[ClassDef[F]]], mutable: Boolean, tpe: Option[Type], body: F[Expr[F]]) extends Term

  /** Tree for expressions
    */
  sealed trait Expr[F[_]] extends Term

  /** Lambda expressions.
    */
  case class LambdaExpr[F[_]](params: List[Symbol[LambdaParam]], tpe: Option[Type], body: F[Expr[F]]) extends Expr[F] {
    def assignType(tpe: Type, body: tpd.Expr): tpd.LambdaExpr =
      Typed(tree = LambdaExpr(params, tpe = this.tpe, body = body), tpe = tpe)
  }

  /** Parameter in the lambda expression.
    * 
    * It is also used to represent class constructor parameters.
    */
  case class LambdaParam(sym: Symbol[LambdaParam], tpe: Type)

  /** Block expressions.
    */
  case class BlockExpr[F[_]](defs: List[F[LocalDef[F]]], expr: F[Expr[F]]) extends Expr[F] {
    def assignType(tpe: Type, defs: List[tpd.LocalDef], expr: tpd.Expr): tpd.BlockExpr =
      Typed(tpe = tpe, tree = BlockExpr(defs, expr))
  }

  /** Identifier that refers to a symbol.
    */
  case class IdentifierExpr[F[_]](sym: Symbol.Ref) extends Expr[F] {
    def assignType(tpe: Type, sym: Symbol[_]): tpd.IdentifierExpr =
      Typed(Trees.IdentifierExpr(Symbol.Ref.Resolved(sym)), tpe = tpe)
  }
  
  case class LiteralIntExpr[F[_]](value: Int) extends Expr[F] {
    def assignType(tpe: Type): tpd.LiteralIntExpr =
      Typed(tree = LiteralIntExpr(value), tpe = tpe)
  }
  
  case class LiteralFloatExpr[F[_]](value: Double) extends Expr[F] {
    def assignType(tpe: Type): tpd.LiteralFloatExpr =
      Typed(tree = LiteralFloatExpr(value), tpe = tpe)
  }
  
  case class LiteralBooleanExpr[F[_]](value: Boolean) extends Expr[F] {
    def assignType(tpe: Type): tpd.LiteralBooleanExpr =
      Typed(tree = LiteralBooleanExpr(value), tpe = tpe)
  }

  /** Application.
    */
  case class ApplyExpr[F[_]](func: F[Expr[F]], args: List[F[Expr[F]]]) extends Expr[F] {
    def assignType(tpe: Type, func: tpd.Expr, args: List[tpd.Expr]): tpd.ApplyExpr =
      Typed(tpe = tpe, tree = ApplyExpr(func = func, args = args))
  }

  /** Selection.
    */
  case class SelectExpr[F[_]](expr: F[Expr[F]], member: Symbol.Ref) extends Expr[F]
  
  case class BinOpExpr[F[_]](op: ExprBinOpType, e1: F[Expr[F]], e2: F[Expr[F]]) extends Expr[F] {
    def assignType(tpe: Type, e1: tpd.Expr, e2: tpd.Expr): tpd.BinOpExpr =
      Typed(tree = BinOpExpr(op, e1, e2), tpe = tpe)
  }
  
  case class UnaryOpExpr[F[_]](op: ExprUnaryOpType, e: F[Expr[F]]) extends Expr[F] {
    def assignType(tpe: Type, e: tpd.Expr): tpd.UnaryOpExpr =
      Typed(tree = UnaryOpExpr(op, e), tpe = tpe)
  }

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
    
    case Assign[F[_]](ref: Symbol.Ref, expr: F[Expr[F]]) extends LocalDef[F]

    def assignTypeBind(body: tpd.Expr): tpd.LocalDefBind = this match {
      case b : Bind[_] =>
        val bind: tpd.LocalDefBind = Typed(tpe = body.tpe, tree = Bind[Typed](Symbol(b.sym.name, null), b.mutable, b.tpe, body))
        bind.tree.sym.dealias = bind
        bind
      case _ => assert(false, s"can not call assignTypeBind on $this")
    }

    def assignTypeEval(expr: tpd.Expr): tpd.LocalDefEval = this match {
      case e : Eval[_] =>
        Typed(tpe = expr.tpe, tree = Eval(expr))
      case _ => assert(false, s"can not call assignTypeEval on $this")
    }

    def assignTypeAssign(sym: Symbol[_], expr: tpd.Expr): tpd.LocalDefAssign = this match {
      case e : Assign[_] =>
        Typed(tpe = expr.tpe, tree = Assign(ref = Symbol.Ref.Resolved(sym), expr))
      case _ => assert(false, s"can not call assignTypeAssign on $this")
    }
  }

  enum ExprBinOpType {
    case +
    case -
    case *
    case /
    case ^
    case &&
    case ||
    case ==
    case !=
    case >=
    case <=
    case >
    case <
  }
  
  enum ExprUnaryOpType {
    case !
    case -
  }

  /** Type projectors to embed extra information into the tree.
    * Currently placeholders.
    */
  case class Untyped[+X](tree: X)
  case class Typed[+X](tree: X, tpe: Type)

  /** Transform tree type to Untyped tree type. For more information, see [[Untyped]].
    */
  type UntypedTree = [T[_[_]]] =>> Untyped[T[Untyped]]

  /** Transform tree type to Typed tree type. For more information, see [[Typed]].
    */
  type TypedTree = [T[_[_]]] =>> Typed[T[Typed]]

  /** Untyped trees.
    */
  object untpd {
    type ClassDef = UntypedTree[Trees.ClassDef]
    type MemberDef = UntypedTree[Trees.MemberDef]
    
    type Expr = UntypedTree[Trees.Expr]
    
    type LiteralIntExpr = UntypedTree[Trees.LiteralIntExpr]
    type LiteralFloatExpr = UntypedTree[Trees.LiteralFloatExpr]
    type LiteralBooleanExpr = UntypedTree[Trees.LiteralBooleanExpr]
    
    type LambdaExpr = UntypedTree[Trees.LambdaExpr]
    
    type BlockExpr = UntypedTree[Trees.BlockExpr]
    type LocalDef = UntypedTree[Trees.LocalDef]
    type LocalDefBind = UntypedTree[Trees.LocalDef.Bind]
    type LocalDefEval = UntypedTree[Trees.LocalDef.Eval]
    
    type ApplyExpr = UntypedTree[Trees.ApplyExpr]
    type SelectExpr = UntypedTree[Trees.SelectExpr]

    type BinOpExpr = UntypedTree[Trees.BinOpExpr]
    type UnaryOpExpr = UntypedTree[Trees.UnaryOpExpr]

    type IdentifierExpr = UntypedTree[Trees.IdentifierExpr]
  }
  
  object tpd {
    type ClassDef = TypedTree[Trees.ClassDef]
    type MemberDef = TypedTree[Trees.MemberDef]
    
    type Expr = TypedTree[Trees.Expr]
    
    type LiteralIntExpr = TypedTree[Trees.LiteralIntExpr]
    type LiteralFloatExpr = TypedTree[Trees.LiteralFloatExpr]
    type LiteralBooleanExpr = TypedTree[Trees.LiteralBooleanExpr]
    
    type LambdaExpr = TypedTree[Trees.LambdaExpr]
    
    type BlockExpr = TypedTree[Trees.BlockExpr]
    type LocalDef = TypedTree[Trees.LocalDef]
    type LocalDefBind = TypedTree[Trees.LocalDef.Bind]
    type LocalDefEval = TypedTree[Trees.LocalDef.Eval]
    type LocalDefAssign = TypedTree[Trees.LocalDef.Assign]

    type BinOpExpr = TypedTree[Trees.BinOpExpr]
    type UnaryOpExpr = TypedTree[Trees.UnaryOpExpr]

    type ApplyExpr = TypedTree[Trees.ApplyExpr]
    type SelectExpr = TypedTree[Trees.SelectExpr]

    type IdentifierExpr = TypedTree[Trees.IdentifierExpr]
  }
}

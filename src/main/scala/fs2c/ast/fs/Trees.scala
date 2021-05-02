package fs2c.ast.fs

import fs2c.ast.Symbol
import fs2c.typer.Types._
import fs2c.codegen.{ CodeBundles => bd }

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
  case class ClassDef[F[_]](sym: Symbol[F[ClassDef[F]]], params: List[Symbol[LambdaParam]], parent: Option[Symbol.Ref], var members: List[F[MemberDef[F]]]) extends Type {
    def assignType(tpe: Type, members: List[tpd.MemberDef]): tpd.ClassDef = {
      val res = Typed(tpe = tpe, tree = Trees.ClassDef[Typed](Symbol(sym.name, null), params, parent, members))
      res.tree.sym.dealias = res
      res
    }

    override def toString: String = s"class ${sym.name}"
  }
  
  extension (cls : tpd.ClassDef) {
    def showTyped: String = {
      val clsDef = cls.tree
      
      s"$clsDef(${clsDef.params}) : { ${clsDef.members map { m => s"${m.tree} : ${m.tpe}" } }"
    }
  }

  /** A member definition in the class body.
    */
  case class MemberDef[F[_]](sym: Symbol[F[MemberDef[F]]], var classDef: Symbol[F[ClassDef[F]]], mutable: Boolean, tpe: Option[Type], body: F[Expr[F]]) extends Term {
    def assignType(tpe: Type, classDef: Symbol[tpd.ClassDef], body: tpd.Expr): tpd.MemberDef = {
      val res = Typed(tpe = tpe, tree = Trees.MemberDef[Typed](Symbol(sym.name, null), classDef, mutable, this.tpe, body))
      res.tree.sym.dealias = res
      
      if body ne null then
        res.freeNames = body.freeNames
      
      res
    }

//    override def toString(): String = s"${sym.name}"
  }

  /** Tree for expressions
    */
  sealed trait Expr[F[_]] extends Term

  /** Lambda expressions.
    */
  case class LambdaExpr[F[_]](params: List[Symbol[LambdaParam]], tpe: Option[Type], body: F[Expr[F]]) extends Expr[F] {
    def assignType(tpe: Type, body: tpd.Expr): tpd.LambdaExpr = setFreeNames(body.freeNames filterNot { n => params contains n }) {
      Typed(tree = LambdaExpr(params, tpe = this.tpe, body = body), tpe = tpe)
    }
  }

  /** Parameter in the lambda expression.
    *
    * It is also used to represent class constructor parameters.
    */
  case class LambdaParam(sym: Symbol[LambdaParam], var tpe: Type, var code: bd.CodeBundle = bd.NoCode)

  /** Block expressions.
    */
  case class BlockExpr[F[_]](defs: List[F[LocalDef[F]]], expr: F[Expr[F]]) extends Expr[F] {
    def boundSymbols(tpdDefs: List[tpd.LocalDef]): List[Symbol[tpd.LocalDefBind]] = tpdDefs flatMap { d =>
      d.tree match {
        case bind: Trees.LocalDef.Bind[Typed] => Some(bind.sym)
        case _ => None
      }
    }

    def assignType(tpe: Type, defs: List[tpd.LocalDef], expr: tpd.Expr): tpd.BlockExpr = setFreeNames {
      val bound = boundSymbols(defs)
      (defs.flatMap(_.freeNames) ++ expr.freeNames) filterNot { sym => bound contains sym }
    } {
      Typed(tpe = tpe, tree = BlockExpr(defs, expr))
    }
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

  case class LiteralStringExpr[F[_]](value: String) extends Expr[F] {
    def assignType(tp: Type): tpd.LiteralStringExpr =
      Typed(tree = LiteralStringExpr(value), tpe = tp)
  }

  /** Application.
    */
  case class ApplyExpr[F[_]](func: F[Expr[F]], args: List[F[Expr[F]]]) extends Expr[F] {
    def assignType(tpe: Type, func: tpd.Expr, args: List[tpd.Expr]): tpd.ApplyExpr =
      setFreeNames(func.freeNames ++ args.flatMap(_.freeNames)) {
        Typed(tpe = tpe, tree = ApplyExpr(func = func, args = args))
      }
  }

  /** Selection.
    */
  case class SelectExpr[F[_]](expr: F[Expr[F]], member: String) extends Expr[F] {
    def assignType(tpe: Type, expr: tpd.Expr): tpd.SelectExpr =
      Typed(tpe = tpe, tree = Trees.SelectExpr(expr, member), freeNames = expr.freeNames)
  }
  
  case class IfExpr[F[_]](cond: F[Expr[F]], trueBody: F[Expr[F]], falseBody: F[Expr[F]]) extends Expr[F] {
    def assignType(tpe: Type, cond: tpd.Expr, trueBody: tpd.Expr, falseBody: tpd.Expr): tpd.IfExpr =
      Typed(
        tpe = tpe,
        tree = Trees.IfExpr(cond, trueBody, falseBody),
        freeNames = cond.freeNames ++ trueBody.freeNames ++ falseBody.freeNames
      )
  }
  
  case class NewExpr[F[_]](ref: Symbol.Ref, params: List[F[Expr[F]]]) extends Expr[F] {
    def assignType(tpe: Type, sym: Symbol[tpd.ClassDef], params: List[tpd.Expr]): tpd.NewExpr =
      Typed(tpe = tpe, tree = NewExpr(Symbol.Ref.Resolved(sym), params))
  }

  case class BinOpExpr[F[_]](op: ExprBinOpType, e1: F[Expr[F]], e2: F[Expr[F]]) extends Expr[F] {
    def assignType(tpe: Type, e1: tpd.Expr, e2: tpd.Expr): tpd.BinOpExpr =
      Typed(tree = BinOpExpr(op, e1, e2), tpe = tpe, freeNames = e1.freeNames ++ e2.freeNames)
  }

  case class UnaryOpExpr[F[_]](op: ExprUnaryOpType, e: F[Expr[F]]) extends Expr[F] {
    def assignType(tpe: Type, e: tpd.Expr): tpd.UnaryOpExpr =
      Typed(tree = UnaryOpExpr(op, e), tpe = tpe, freeNames = e.freeNames)
  }

  /** Ground values in the language.
    *
    * @tparam F
    */
  sealed trait GroundValue[F[_]] extends Expr[F] {
    /** Self type.
      *
      * @tparam F
      */
    type This[F[_]] <: GroundValue[F]

    /** Get a untyped tree for the ground value.
      *
      * @return
      */
    def untyped: UntypedTree[This]

    /** Get a typed tree for the ground value.
      *
      * @return
      */
    def typed: TypedTree[This]
  }

  /** Built-in function `readInt : () => Int`.
    *
    * @tparam F
    */
  case class ReadInt[F[_]]() extends GroundValue[F] {
    override type This[F[_]] = ReadInt[F]

    override def untyped: UntypedTree[ReadInt] = Untyped(ReadInt())

    override def typed: TypedTree[ReadInt] = Typed(ReadInt(), tpe = LambdaType(Nil, GroundType.IntType))
  }

  /** Built-in function `readFloat : () => Float`.
    *
    * @tparam F
    */
  case class ReadFloat[F[_]]() extends GroundValue[F] {
    override type This[F[_]] = ReadFloat[F]
    override def untyped: UntypedTree[ReadFloat] = Untyped(ReadFloat())
    override def typed: TypedTree[ReadFloat] = Typed(ReadFloat(), tpe = LambdaType(Nil, GroundType.FloatType))
  }

  case class ReadStr[F[_]]() extends GroundValue[F] {
    override type This[F[_]] = ReadStr[F]
    override def untyped: UntypedTree[ReadStr] = Untyped(ReadStr())
    override def typed: TypedTree[ReadStr] = Typed(ReadStr(), tpe = LambdaType(Nil, GroundType.StringType))
  }

  /** Built-in function `printInt : Int => Int`.
    *
    * @tparam F
    */
  case class PrintInt[F[_]]() extends GroundValue[F] {
    override type This[F[_]] = PrintInt[F]

    override def untyped: UntypedTree[PrintInt] = Untyped(PrintInt())

    override def typed: TypedTree[PrintInt] = Typed(PrintInt(), tpe = LambdaType(List(GroundType.IntType), GroundType.IntType))
  }

  case class PrintLnInt[F[_]]() extends GroundValue[F] {
    override type This[F[_]] = PrintLnInt[F]

    override def untyped: UntypedTree[PrintLnInt] = Untyped(PrintLnInt())

    override def typed: TypedTree[PrintLnInt] = Typed(PrintLnInt(), tpe = LambdaType(List(GroundType.IntType), GroundType.IntType))
  }

  /** Built-in function `printFloat : Float => Float`.
    *
    * @tparam F
    */
  case class PrintFloat[F[_]]() extends GroundValue[F] {
    override type This[F[_]] = PrintFloat[F]

    override def untyped: UntypedTree[PrintFloat] = Untyped(PrintFloat())

    override def typed: TypedTree[PrintFloat] = Typed(PrintFloat(), tpe = LambdaType(List(GroundType.FloatType), GroundType.FloatType))
  }

  case class PrintLnFloat[F[_]]() extends GroundValue[F] {
    override type This[F[_]] = PrintLnFloat[F]

    override def untyped: UntypedTree[PrintLnFloat] = Untyped(PrintLnFloat())

    override def typed: TypedTree[PrintLnFloat] = Typed(PrintLnFloat(), tpe = LambdaType(List(GroundType.FloatType), GroundType.FloatType))
  }

  case class Print[F[_]]() extends GroundValue[F] {
    override type This[F[_]] = Print[F]
    override def untyped: UntypedTree[Print] = Untyped(Print())
    override def typed: TypedTree[Print] = Typed(Print(), tpe = LambdaType(List(GroundType.StringType), GroundType.StringType))
  }

  case class PrintLn[F[_]]() extends GroundValue[F] {
    override type This[F[_]] = PrintLn[F]
    override def untyped: UntypedTree[PrintLn] = Untyped(PrintLn())
    override def typed: TypedTree[PrintLn] = Typed(PrintLn(), tpe = LambdaType(List(GroundType.StringType), GroundType.StringType))
  }

  case class InitRandom[F[_]]() extends GroundValue[F] {
    override type This[F[_]] = InitRandom[F]
    override def untyped: UntypedTree[InitRandom] = Untyped(InitRandom())
    override def typed: TypedTree[InitRandom] = Typed(InitRandom(), tpe = LambdaType(Nil, GroundType.IntType))
  }

  case class NextRandom[F[_]]() extends GroundValue[F] {
    override type This[F[_]] = NextRandom[F]
    override def untyped: UntypedTree[NextRandom] = Untyped(NextRandom())
    override def typed: TypedTree[NextRandom] = Typed(NextRandom(), tpe = GroundType.IntType)
  }

  val groundValueMap: Map[String, UntypedTree[GroundValue]] = Map(
    "readInt" -> ReadInt().untyped,
    "readFloat" -> ReadFloat().untyped,
    "readStr" -> ReadStr().untyped,
    "printInt" -> PrintInt().untyped,
    "printFloat" -> PrintFloat().untyped,
    "printlnInt" -> PrintLnInt().untyped,
    "printlnFloat" -> PrintLnFloat().untyped,
    "print" -> Print().untyped,
    "println" -> PrintLn().untyped,
    "initRandom" -> InitRandom().untyped,
    "nextRandom" -> NextRandom().untyped,
  )

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

    case While[F[_]](cond: F[Expr[F]], body: F[Expr[F]]) extends LocalDef[F]

    def assignTypeBind(body: tpd.Expr): tpd.LocalDefBind = this match {
      case b : Bind[_] =>
        val bind: tpd.LocalDefBind =
          Typed(
            tpe = body.tpe,
            tree = Bind[Typed](Symbol(b.sym.name, null), b.mutable, b.tpe, body),
            freeNames = body.freeNames
          )
        bind.tree.sym.dealias = bind
        bind
      case _ => assert(false, s"can not call assignTypeBind on $this")
    }

    /** Checks whether this local definition binds a lambda expr.
      */
    def isLambdaBind: Boolean = this match {
      case b : Bind[_] => b.body match {
        case tpt : Typed[_] => tpt.tree match {
          case _ : LambdaExpr[_] => true
          case _ => false
        }
        case untpt : Untyped[_] => untpt.tree match {
          case _ : LambdaExpr[_] => true
          case _ => false
        }
      }
      case _ => false
    }

    def assignTypeEval(expr: tpd.Expr): tpd.LocalDefEval = this match {
      case e : Eval[_] =>
        Typed(tpe = expr.tpe, tree = Eval(expr), freeNames = expr.freeNames)
      case _ => assert(false, s"can not call assignTypeEval on $this")
    }

    def assignTypeAssign(sym: Symbol[_], expr: tpd.Expr): tpd.LocalDefAssign = this match {
      case e : Assign[_] =>
        Typed(
          tpe = expr.tpe,
          tree = Assign(ref = Symbol.Ref.Resolved(sym), expr),
          freeNames = expr.freeNames  // need to further check whether the assigned sym is
                                      // free, since this piece of info will not be available
                                      // in Types.scala
        )
      case _ => assert(false, s"can not call assignTypeAssign on $this")
    }

    def assignTypeWhile(cond: tpd.Expr, body: tpd.Expr): tpd.LocalDefWhile = this match {
      case e : While[_] =>
        Typed(
          tpe = body.tpe,
          tree = While(cond = cond, body = body),
          freeNames = cond.freeNames ++ body.freeNames
        )
      case _ => assert(false, s"can not call assignTypeWhile on $this")
    }
  }

  enum ExprBinOpType {
    case +
    case -
    case *
    case /
    case ^
    case %
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

  /** Wrapper for untyped trees.
    */
  final case class Untyped[+X](tree: X)

  /** Wrapper for typed trees.
    * 
    * @param tree Wrapped tree.
    * @param tpe Type of the tree.
    * @param code Generated code bundle for the tree.
    * @param freeNames Free names of the tree.
    */
  final case class Typed[+X](tree: X, var tpe: Type, 
                             var code: bd.CodeBundle = bd.NoCode, 
                             var freeNames: List[Symbol[_]] = Nil) {
    def type_=(tpe: Type) =
      this.tpe = tpe
      
    def assignCode[T <: bd.CodeBundle](assignCode: X => T): T = {
      val gen = assignCode(tree)
      code = gen
      gen
    }
  }

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
    type LiteralStringExpr = UntypedTree[Trees.LiteralStringExpr]

    type LambdaExpr = UntypedTree[Trees.LambdaExpr]
    
    type BlockExpr = UntypedTree[Trees.BlockExpr]
    type LocalDef = UntypedTree[Trees.LocalDef]
    type LocalDefBind = UntypedTree[Trees.LocalDef.Bind]
    type LocalDefEval = UntypedTree[Trees.LocalDef.Eval]
    type LocalDefWhile = UntypedTree[Trees.LocalDef.While]
    
    type ApplyExpr = UntypedTree[Trees.ApplyExpr]
    type SelectExpr = UntypedTree[Trees.SelectExpr]

    type BinOpExpr = UntypedTree[Trees.BinOpExpr]
    type UnaryOpExpr = UntypedTree[Trees.UnaryOpExpr]

    type IfExpr = UntypedTree[Trees.IfExpr]

    type NewExpr = UntypedTree[Trees.NewExpr]

    type IdentifierExpr = UntypedTree[Trees.IdentifierExpr]

    type GroundValue = UntypedTree[Trees.GroundValue]
  }

  object tpd {
    type ClassDef = TypedTree[Trees.ClassDef]
    type MemberDef = TypedTree[Trees.MemberDef]
    
    type Expr = TypedTree[Trees.Expr]
    
    type LiteralIntExpr = TypedTree[Trees.LiteralIntExpr]
    type LiteralFloatExpr = TypedTree[Trees.LiteralFloatExpr]
    type LiteralBooleanExpr = TypedTree[Trees.LiteralBooleanExpr]
    type LiteralStringExpr = TypedTree[Trees.LiteralStringExpr]

    type LambdaExpr = TypedTree[Trees.LambdaExpr]
    
    type BlockExpr = TypedTree[Trees.BlockExpr]
    type LocalDef = TypedTree[Trees.LocalDef]
    type LocalDefBind = TypedTree[Trees.LocalDef.Bind]
    type LocalDefEval = TypedTree[Trees.LocalDef.Eval]
    type LocalDefAssign = TypedTree[Trees.LocalDef.Assign]
    type LocalDefWhile = TypedTree[Trees.LocalDef.While]

    type BinOpExpr = TypedTree[Trees.BinOpExpr]
    type UnaryOpExpr = TypedTree[Trees.UnaryOpExpr]

    type ApplyExpr = TypedTree[Trees.ApplyExpr]
    type SelectExpr = TypedTree[Trees.SelectExpr]
    
    type IfExpr = TypedTree[Trees.IfExpr]
    
    type NewExpr = TypedTree[Trees.NewExpr]

    type IdentifierExpr = TypedTree[Trees.IdentifierExpr]

    type GroundValue = TypedTree[Trees.GroundValue]
  }

  def setFreeNames[X](freeNames: List[Symbol[_]])(tpdTree: Typed[X]): Typed[X] = {
    tpdTree.freeNames = freeNames
    tpdTree
  }

  trait TreeTraverse[F[+_], G[+_]] {
    type TreeType = [X[_[_]]] =>> F[X[F]]
    
    def unwrapExpr(expr: TreeType[Expr]): Expr[F]
    
    def unwrapLocalDef(localDef: TreeType[LocalDef]): LocalDef[F]
    
    def traverseExpr(expr: TreeType[Expr]): G[TreeType[Expr]] = unwrapExpr(expr) match {
      case LambdaExpr(params, tpe, body) =>
        traverseLambdaExpr(params, tpe, traverseExpr(body))
      case BlockExpr(defs, expr) =>
        traverseBlockExpr(defs map traverseLocalDef, traverseExpr(expr))
      case IdentifierExpr(sym) =>
        traverseIdentifierExpr(sym)
      case LiteralIntExpr(value) =>
        traverseLiteralIntExpr(value)
      case LiteralFloatExpr(value) =>
        traverseLiteralFloatExpr(value)
      case LiteralBooleanExpr(value) =>
        traverseLiteralBooleanExpr(value)
      case ApplyExpr(func, args) =>
        traverseApplyExpr(traverseExpr(func), args map traverseExpr)
      case SelectExpr(expr, member) =>
        traverseSelectExpr(traverseExpr(expr), member)
      case BinOpExpr(op, e1, e2) =>
        traverseBinOpExpr(op, traverseExpr(e1), traverseExpr(e2))
      case UnaryOpExpr(op, e) =>
        traverseUnaryOpExpr(op, traverseExpr(e))
    }
    
    def traverseLocalDef(localDef: TreeType[LocalDef]): G[TreeType[LocalDef]] = unwrapLocalDef(localDef) match {
      case LocalDef.Assign(ref, body) =>
        traverseLocalDefAssign(ref, traverseExpr(body))
      case LocalDef.Bind(sym, mutable, tpe, body) =>
        traverseLocalDefBind(sym, mutable, tpe, traverseExpr(body))
      case LocalDef.Eval(expr) =>
        traverseLocalDefEval(traverseExpr(expr))
    }
    
    def traverseLambdaExpr(params: List[Symbol[LambdaParam]], tpe: Option[Type], body: G[TreeType[Expr]]): G[TreeType[LambdaExpr]]
    
    def traverseBlockExpr(defs: List[G[TreeType[LocalDef]]], expr: G[TreeType[Expr]]): G[TreeType[BlockExpr]]
    
    def traverseIdentifierExpr(symRef: Symbol.Ref): G[TreeType[IdentifierExpr]]
    
    def traverseLiteralIntExpr(value: Int): G[TreeType[LiteralIntExpr]]
    
    def traverseLiteralFloatExpr(value: Double): G[TreeType[LiteralFloatExpr]]
    
    def traverseLiteralBooleanExpr(value: Boolean): G[TreeType[LiteralBooleanExpr]]
    
    def traverseApplyExpr(func: G[TreeType[Expr]], args: List[G[TreeType[Expr]]]): G[TreeType[ApplyExpr]]

    def traverseSelectExpr(expr: G[TreeType[Expr]], member: String): G[TreeType[SelectExpr]]
    
    def traverseBinOpExpr(op: ExprBinOpType, e1: G[TreeType[Expr]], e2: G[TreeType[Expr]]): G[TreeType[BinOpExpr]]
    
    def traverseUnaryOpExpr(op: ExprUnaryOpType, e: G[TreeType[Expr]]): G[TreeType[UnaryOpExpr]]

    def traverseLocalDefBind(sym: Symbol[TreeType[LocalDef.Bind]], mutable: Boolean, tpe: Option[Type], body: G[TreeType[Expr]]): G[TreeType[LocalDef.Bind]]
    
    def traverseLocalDefEval(expr: G[TreeType[Expr]]): G[TreeType[LocalDef.Eval]]
    
    def traverseLocalDefAssign(ref: Symbol.Ref, expr: G[TreeType[Expr]]): G[TreeType[LocalDef.Assign]]
  }
}

package fs2c.typer

import fs2c.ast.Symbol
import fs2c.ast.Scopes._
import fs2c.ast.fs._
import Trees.{LocalDef, Typed, Untyped, tpd, untpd, ExprBinOpType => bop, ExprUnaryOpType => uop}
import Types._
import GroundType._

import fs2c.tools.Unique
import Unique.freshTypeVar

/** Typer for Featherweight Scala.
  */
class Typer {
  import Typer.TypeError

  val scopeCtx: ScopeContext = new ScopeContext
  
  val constrs = new Constraints.ConstraintSolver

  /** Records all expressions that have been typed.
    */
  var allTyped: List[Typed[_]] = Nil
  
  def recordTyped[X](tpd: => Typed[X]): Typed[X] = {
    allTyped = tpd :: allTyped
    tpd
  }

  def recordEquality(tpe1: Type, tpe2: Type): Unit =
    constrs.addEquality(tpe1, tpe2)

  /** Fresh type variable prefixed with T.
    */
  val freshTvar: TypeVariable =
    freshTypeVar(prefix = "T")

  /** Fresh type variable prefixed with X.
    */
  val freshXvar: TypeVariable =
    freshTypeVar(prefix = "X")

  /** Type class definitions.
    */
  def typedClassDef(classDef: untpd.ClassDef): tpd.ClassDef = ???
  

  /** Type expressions.
    */
  def typedExpr(expr: untpd.Expr): tpd.Expr = recordTyped {
    expr.tree match {
      case x : Trees.LiteralIntExpr[_] => x.assignType(IntType)
      case x : Trees.LiteralFloatExpr[_] => x.assignType(FloatType)
      case x : Trees.LiteralBooleanExpr[_] => x.assignType(BooleanType)
      case x : Trees.BinOpExpr[Untyped] => typedBinOpExpr(Untyped(x))
      case x : Trees.UnaryOpExpr[Untyped] => typedUnaryOpExpr(Untyped(x))
      case x : Trees.LambdaExpr[Untyped] => typedLambdaExpr(Untyped(x))
      case x : Trees.IdentifierExpr[Untyped] => typedIdentifierExpr(Untyped(x))
      case x : Trees.BlockExpr[Untyped] => typedBlockExpr(Untyped(x))
      case x : Trees.ApplyExpr[Untyped] => typedApplyExpr(Untyped(x))
      case _ => throw TypeError(s"can not type $expr : lacking implementation")
    }
  }

  /** Type lambda expressions.
    */
  def typedLambdaExpr(expr: untpd.LambdaExpr): tpd.LambdaExpr = {
    val lambda: Trees.LambdaExpr[Untyped] = expr.tree

    // --- Open a new scope ---
    scopeCtx.locateScope()

    // add lambda parameters into the scope
    lambda.params map { sym => scopeCtx.addSymbol(sym) }
    // typing the body
    val typedBody: tpd.Expr = typedExpr(lambda.body)
    lambda.tpe match {
      case None => ()
      case Some(tpe) =>
        if tpe != typedBody.tpe then
          throw TypeError(s"Expected lambda body type: $tpe, given: ${typedBody.tpe}")
        else
          ()
    }

    // --- Close the scope ---
    scopeCtx.relocateScope()

    lambda.assignType(
      tpe = LambdaType(lambda.params map { sym => sym.dealias.tpe }, typedBody.tpe),
      body = typedBody
    )
  }

  /** Type block expressions.
    */
  def typedBlockExpr(expr: untpd.BlockExpr): tpd.BlockExpr = {
    val block: Trees.BlockExpr[Untyped] = expr.tree

    // --- Open a new scope ---
    scopeCtx.locateScope()

    val defs: List[tpd.LocalDef] = block.defs map typedLocalDef
    val tpdExpr: tpd.Expr = typedExpr(block.expr)

    // --- Close the scope ---
    scopeCtx.relocateScope()

    block.assignType(tpdExpr.tpe, defs, tpdExpr)
  }

  def typedLocalDef(expr: untpd.LocalDef): tpd.LocalDef = {
    val localDef: Trees.LocalDef[Untyped] = expr.tree

    localDef match {
      case bind @ Trees.LocalDef.Bind(sym, mutable, tpe, body) =>
        val typedBody = typedExpr(body)
        tpe match {
          case Some(tpe) if tpe != typedBody.tpe =>
            throw TypeError(s"Type mismatch: expecting $tpe but found ${typedBody.tpe}")
          case _ => ()
        }
        val typedBind: tpd.LocalDefBind = bind.assignTypeBind(typedBody)
        scopeCtx.addSymbol(typedBind.tree.sym)

        typedBind
      case eval @ Trees.LocalDef.Eval(expr) =>
        eval.assignTypeEval(typedExpr(expr))
      case assign @ Trees.LocalDef.Assign(symRef, expr) => {
        val sym: Symbol[_] = symRef match {
          case Symbol.Ref.Resolved(sym) =>
            scopeCtx.findSym(sym.name) match {
              case None =>
                throw TypeError(s"unknown symbol: ${sym.name}")
              case Some(sym) => sym
            }
          case Symbol.Ref.Unresolved(symName) =>
            scopeCtx.findSym(symName) match {
              case None =>
                throw TypeError(s"unknown symbol: $symName")
              case Some(sym) => sym
            }
        }
        val tpdExpr: tpd.Expr = typedExpr(expr)
        if isSymbolMutable(sym) && tpdExpr.tpe == typeOfSymbol(sym) then
          assign.assignTypeAssign(sym, tpdExpr)
        else {
          if !isSymbolMutable(sym) then
            throw TypeError(s"can not assign to an immutable $sym")
          else
            throw TypeError(s"can not assign value of ${tpdExpr.tpe} to $sym")
        }
      }
    }
  }

  /** Checks whether a symbol is mutable.
    */
  def isSymbolMutable(sym: Symbol[_]): Boolean = {
    sym.dealias match {
      case typed : Typed[_] =>
        val tree = typed.tree
        tree match {
          case bind : Trees.LocalDef.Bind[_] => bind.mutable
          case member : Trees.MemberDef[_] => member.mutable
          case _ => false
        }
      case _ => false
    }
  }

  def typeOfSymbol(sym: Symbol[_]): Type = sym.dealias match {
    case tped : Typed[_] => tped.tpe
    case param : Trees.LambdaParam => param.tpe
  }

  def typedIdentifierExpr(expr: untpd.IdentifierExpr): tpd.IdentifierExpr =
    expr.tree.sym match {
      case Symbol.Ref.Unresolved(symName) =>
        scopeCtx.findSym(symName) match {
          case None =>
            throw TypeError(s"unknown symbol: $symName")
          case Some(sym) =>
            expr.tree.assignType(typeOfSymbol(sym), sym)
        }
      case Symbol.Ref.Resolved(sym) =>
        scopeCtx.findSym(sym.name) match {
          case None =>
            throw TypeError(s"unknown resolved symbol: ${sym.name}, this is caused by a bug in the compiler.")
          case Some(sym) =>
            expr.tree.assignType(typeOfSymbol(sym), sym)
        }
    }

  def typedApplyExpr(expr: untpd.ApplyExpr): tpd.ApplyExpr = {
    def apply = expr.tree
    val func: tpd.Expr = typedExpr(apply.func)

    func.tpe match {
      case LambdaType(expectParamTypes, valueType) =>
        val params: List[tpd.Expr] = apply.args map typedExpr
        val paramTypes: List[Type] = params map (_.tpe)
        @annotation.tailrec def go(ts1: List[Type], ts2: List[Type]): Unit = (ts1, ts2) match {
          case (Nil, Nil) => ()
          case (Nil, _) | (_, Nil) =>
            throw TypeError(s"parameter number mismatch.")
          case (t1 :: ts1, t2 :: ts2) if t1 == t2 =>
            go(ts1, ts2)
          case (t1 :: ts1, t2 :: ts2) =>
            throw TypeError(s"arugment type mismatch: $t1 and $t2")
        }
        // do arugment type checking
        go(expectParamTypes, paramTypes)
        apply.assignType(tpe = valueType, func, params)
      case _ =>
        throw TypeError(s"can not apply expr of type ${func.tpe} : is not a function")
    }
  }

  /** Type binary operator expressions.
    */
  def typedBinOpExpr(expr: untpd.BinOpExpr): tpd.BinOpExpr = {
    val op = expr.tree.op
    val e1 = typedExpr(expr.tree.e1)
    val e2 = typedExpr(expr.tree.e2)
    val tpe = binOpSig get op match {
      case None => throw TypeError(s"unknown binary operator: $op")
      case Some(sigs) => sigs.collectFirst {
        case sig if sig.infer(e1.tpe, e2.tpe).isDefined => sig.infer(e1.tpe, e2.tpe).get
      }
    } match {
      case None => throw TypeError(s"could not find override function for $op with operand type ${e1.tpe} and ${e2.tpe}")
      case Some(tpe) => tpe
    }

    expr.tree.assignType(tpe, e1, e2)
  }

  /** Type unary operator expressions.
   */
  def typedUnaryOpExpr(expr: untpd.UnaryOpExpr): tpd.UnaryOpExpr = {
    val unaryExpr: Trees.UnaryOpExpr[Untyped] = expr.tree
    val op = unaryExpr.op
    val e = typedExpr(unaryExpr.e)
    val tpe = unaryOpSig get op match {
      case None => throw TypeError(s"unknown unary operator: $op")
      case Some(sigs) => sigs collectFirst {
        case sig if sig.infer(e.tpe).isDefined => sig.infer(e.tpe).get
      }
    } match {
      case None => throw TypeError(s"could not find signature for $op with operand type ${e.tpe}")
      case Some(tpe) => tpe
    }

    unaryExpr.assignType(tpe = tpe, e = e)
  }


  /** Signature for binary operators.
    */
  trait BinOpSig {
    /** Check and inference the result type of the binary operation.
      *
      * @param e1 Type for the left operand.
      * @param e2 Type for the right operand.
      * @return Inferred result type. `None` if operand type mismatch.
      */
    def infer(e1: Type, e2: Type): Option[Type]
  }

  /** Concrete signature with fixed operand and result types.
    */
  case class ConcreteBinOpSig(e1: Type, e2: Type, res: Type) extends BinOpSig {
    override def infer(e1: Type, e2: Type): Option[Type] =
      if e1 == this.e1 && e2 == this.e2 then
        Some(res)
      else
        None
  }

  /** Signature for unary operators.
    */
  trait UnaryOpSig {
    def infer(e: Type): Option[Type]
  }

  case class ConcreteUnaryOpSig(e: Type, res: Type) extends UnaryOpSig {
    override def infer(e: Type): Option[Type] =
      if e == this.e then
        Some(res)
      else
        None
  }

  /** Signature for built-in binary operators.
    */
  val binOpSig: Map[bop, List[BinOpSig]] = Map(
    bop.+ -> List(
      ConcreteBinOpSig(IntType, IntType, IntType),
      ConcreteBinOpSig(FloatType, FloatType, FloatType),
    ),
    bop.- -> List(
      ConcreteBinOpSig(IntType, IntType, IntType),
      ConcreteBinOpSig(FloatType, FloatType, FloatType),
    ),
    bop.* -> List(
      ConcreteBinOpSig(IntType, IntType, IntType),
      ConcreteBinOpSig(FloatType, FloatType, FloatType),
    ),
    bop./ -> List(
      ConcreteBinOpSig(IntType, IntType, IntType),
      ConcreteBinOpSig(FloatType, FloatType, FloatType),
    ),
    bop.^ -> List(
      ConcreteBinOpSig(IntType, IntType, IntType),
      ConcreteBinOpSig(FloatType, FloatType, FloatType),
    ),
    bop.> -> List(
      ConcreteBinOpSig(IntType, IntType, BooleanType),
      ConcreteBinOpSig(FloatType, FloatType, BooleanType),
    ),
    bop.< -> List(
      ConcreteBinOpSig(IntType, IntType, BooleanType),
      ConcreteBinOpSig(FloatType, FloatType, BooleanType),
    ),
    bop.>= -> List(
      ConcreteBinOpSig(IntType, IntType, BooleanType),
      ConcreteBinOpSig(FloatType, FloatType, BooleanType),
    ),
    bop.<= -> List(
      ConcreteBinOpSig(IntType, IntType, BooleanType),
      ConcreteBinOpSig(FloatType, FloatType, BooleanType),
    ),
    bop.&& -> List(
      ConcreteBinOpSig(BooleanType, BooleanType, BooleanType),
    ),
    bop.|| -> List(
      ConcreteBinOpSig(BooleanType, BooleanType, BooleanType),
    ),
    bop.== -> List(
      new BinOpSig {
        override def infer(e1: Type, e2: Type): Option[Type] =
          if e1 == e2 then
            Some(BooleanType)
          else
            None
      }
    ),
    bop.!= -> List(
      new BinOpSig {
        override def infer(e1: Type, e2: Type): Option[Type] =
          if e1 == e2 then
            Some(BooleanType)
          else
            None
      }
    ),
  )

  /** Signature for unary operators.
    */
  val unaryOpSig: Map[uop, List[UnaryOpSig]] = Map(
    uop.! -> List(
      ConcreteUnaryOpSig(BooleanType, BooleanType)
    ),
    uop.- -> List(
      ConcreteUnaryOpSig(IntType, IntType),
      ConcreteUnaryOpSig(FloatType, FloatType),
    ),
  )
}

object Typer {
  case class TypeError(msg: String) extends Exception(msg)
  
  def showTypedExpr(expr: tpd.Expr, indentLevel: Int = 0): String = {
    val tree: Trees.Expr[Typed] = expr.tree
    val tpe: Type = expr.tpe
    def showLambdaParam(param: Trees.LambdaParam): String =
      s"${param.sym.name} : ${param.tpe}"
    def showLocalDef(localDef: tpd.LocalDef): String = {
      val tree: Trees.LocalDef[Typed] = localDef.tree
      val tpe: Type = localDef.tpe
      tree match {
        case bind : Trees.LocalDef.Bind[Typed] =>
          val kw = if bind.mutable then "var" else "val"
          s"$kw ${bind.sym.name} : $tpe = ${showTypedExpr(bind.body, indentLevel)}"
        case eval : Trees.LocalDef.Eval[Typed] =>
          s"${showTypedExpr(eval.expr, indentLevel)}"
        case assign : Trees.LocalDef.Assign[Typed] =>
          s"${assign.ref} = ${showTypedExpr(assign.expr, indentLevel)}"
      }
    }
    tree match {
      case Trees.LambdaExpr(params, _, body) =>
        s"((${params map { sym => showLambdaParam(sym.dealias) } mkString ", "}) => ${showTypedExpr(body, indentLevel)}) : $tpe"
      case Trees.BlockExpr(defs, expr) =>
        s"{ ${defs map showLocalDef mkString "; "}; ${showTypedExpr(expr)} } : $tpe"
      case Trees.IdentifierExpr(sym) =>
        sym.toString + s" : $tpe"
      case Trees.LiteralIntExpr(value) =>
        value.toString + s" : $tpe"
      case Trees.LiteralFloatExpr(value) =>
        value.toString + s" : $tpe"
      case Trees.LiteralBooleanExpr(value) =>
        value.toString + s" : $tpe"
      case Trees.ApplyExpr(func, args) =>
        s"${showTypedExpr(func)} : $tpe"
      case Trees.SelectExpr(expr, member) =>
        s"(${showTypedExpr(expr)}.$member : $tpe)"
      case Trees.BinOpExpr(op, e1, e2) =>
        s"($op ${showTypedExpr(e1)} ${showTypedExpr(e2)}) : $tpe"
      case Trees.UnaryOpExpr(op, e) => ???
    }
  }
}

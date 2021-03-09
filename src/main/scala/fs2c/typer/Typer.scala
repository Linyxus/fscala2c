package fs2c.typer

import fs2c.ast.Symbol
import fs2c.ast.Scopes._
import fs2c.ast.fs._
import Trees.{tpd, untpd, Typed, Untyped, ExprBinOpType => bop}
import Types._
import GroundType._

class Typer {
  case class TypeError(msg: String) extends Exception(msg)
  
  val scopeCtx: ScopeContext = new ScopeContext

  /** Type expressions.
    */
  def typedExpr(expr: untpd.Expr): tpd.Expr = expr.tree match {
    case x : Trees.LiteralIntExpr[_] => x.assignType(IntType)
    case x : Trees.LiteralFloatExpr[_] => x.assignType(FloatType)
    case x : Trees.LiteralBooleanExpr[_] => x.assignType(BooleanType)
    case x : Trees.BinOpExpr[Untyped] => typedBinOpExpr(Untyped(x))
    case x : Trees.LambdaExpr[Untyped] => typedLambdaExpr(Untyped(x))
    case x : Trees.IdentifierExpr[Untyped] => typedIdentifierExpr(Untyped(x))
    case x : Trees.BlockExpr[Untyped] => typedBlockExpr(Untyped(x))
    case _ => throw TypeError(s"can not type $expr : lacking implementation")
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
}

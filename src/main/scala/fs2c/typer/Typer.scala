package fs2c.typer

import fs2c.ast.Symbol
import fs2c.ast.Scopes._
import fs2c.ast.fs._
import Trees.{LocalDef, Typed, Untyped, tpd, untpd, ExprBinOpType => bop, ExprUnaryOpType => uop}
import Types._
import GroundType._
import Constraints._

import fs2c.tools.Unique
import Unique.{freshTypeVar, uniqueName}

/** Typer for Featherweight Scala.
  */
class Typer {
  import Typer.TypeError

  case class TypingScope(var allTyped: List[Typed[_]], parent: TypingScope)

  val scopeCtx: ScopeContext = new ScopeContext

  val constrs = new ConstraintSolver

  private def solvedSubst: Substitution = constrs.solve

  /** Records all expressions that have been typed.
    */
  var typingScope: TypingScope = TypingScope(Nil, null)

  def typedInBlock: List[Typed[_]] = typingScope.allTyped

  def forceInstantiateBlock(): Unit =
    typedInBlock foreach { tpd => forceInstantiate(tpd) }

  def recordTyped[X](tpd: Typed[X]): Typed[X] = {
    typingScope.allTyped = tpd :: typingScope.allTyped
    tpd
  }

  def locateTypingScope(): Unit =
    typingScope = TypingScope(Nil, typingScope)

  def relocateTypingScope(): Unit = {
    assert(typingScope.parent ne null, "can not relocate from the root typing scope.")
    typingScope = typingScope.parent
  }

  def recordEquality(tpe1: Type, tpe2: Type): Unit = {
    constrs.addEquality(tpe1, tpe2)
  }

  /** Fresh type variable prefixed with T.
    */
  def freshTvar: TypeVariable =
    freshTypeVar(prefix = "T")

  /** Fresh type variable prefixed with X.
    */
  def freshXvar: TypeVariable =
    freshTypeVar(prefix = "X")
  
  def clsVar(clsDef: tpd.ClassDef): ClassTypeVariable =
    ClassTypeVariable(clsDef, Nil)

  /** Perform force instantiation of the type.
    * Will throw a error if there are type variable that can not be
    * instantiated in the type.
    */
  def forceInstantiate[X](tpd: Typed[X]): Typed[X] = {
    val instType = solvedSubst.instantiateType(tpd.tpe)
    instType match {
      case None =>
        throw TypeError(s"Can not instantiate type variable ${tpd.tpe} when typing ${tpd.tree}")
      case Some(tpe) =>
        tpd.tpe = tpe
        tpd
    }
  }

  def tryInstantiateType(tpe: Type): Option[Type] =
    solvedSubst.instantiateType(tpe)
  
  def forceInstantiateType(tpe: Type): Type =
    tryInstantiateType(tpe) match {
      case None =>
        throw TypeError(s"Can not instantiate type variable $tpe")
      case Some(t) => t
    }

  /** Instantiate class type variable. Fails if the class definition does not conform to the predicates.
    */
  def instantiateClassTypeVariable(clsVar: ClassTypeVariable): Type = {
    import Predicate._
    val expectTypes: Map[String, Type] = (clsVar.predicates map { case HaveMemberOfType(mem, t) => mem -> forceInstantiateType(t) }).toMap
    val realTypes: Map[String, Type] = (clsVar.classDef.tree.members map { x => x.tree.sym.name -> forceInstantiateType(x.tpe) }).toMap
    
    if !(expectTypes.keySet subsetOf realTypes.keySet) then
      throw TypeError(s"can not conform required member set ${expectTypes.keySet} with actual set ${realTypes.keySet}")

    expectTypes.keys map { key =>
      val expect : Type = expectTypes(key)
      val real : Type = realTypes(key)
      if expect != real then
        throw TypeError(s"unmatched type for member $key, expected $expect but found $real")
    }
    
    clsVar.classDef.tree
  }

  /** Type class definitions.
    */
  def typedClassDef(classDef: untpd.ClassDef): tpd.ClassDef = {
    val clsDef: Trees.ClassDef[Untyped] = classDef.tree
    
    // --- locate a new scope ---
    scopeCtx.locateScope()
    locateTypingScope()
    
    // tpd.ClassDef <--> ClassTypeVariable
    val typedClsDef: tpd.ClassDef = clsDef.assignType(null, null)
    val clsTvar: ClassTypeVariable = clsVar(typedClsDef)
    typedClsDef.tpe = clsTvar
    
    // introduce placeholder definitions into scope
    val untypedMemDefs: List[Trees.MemberDef[Untyped]] = clsDef.members map (_.tree)
    var placeholders: Map[String, Symbol[tpd.MemberDef]] = Map.empty
    untypedMemDefs foreach { memDef =>
      val tpe: Type = memDef.tpe getOrElse freshXvar
      val typed: tpd.MemberDef = memDef.assignType(tpe, typedClsDef.tree.sym, null)
      scopeCtx.addSymbol(typed.tree.sym)
      placeholders = placeholders.updated(typed.tree.sym.name, typed.tree.sym)
    }
    
    // typing the member definitions
    val typedMemDefs: List[tpd.MemberDef] = clsDef.members map typedMemberDef
    
    typedMemDefs foreach {
      case tpdDef @ Typed(d : Trees.MemberDef[Typed], actualType) =>
        placeholders get d.sym.name match {
          case None =>
            throw TypeError(s"fatal error: member name not found in placeholders. This is caused by a bug.")
          case Some(sym) =>
            val assumedType: Type = sym.dealias.tpe
            recordEquality(assumedType, actualType)
            sym.dealias = tpdDef
            tpdDef.tree.classDef = typedClsDef.tree.sym
        }
    }
    
    /** forcefully instantiate all type variables
      * note: this will not instantiate the currently typing ClassDef */
    forceInstantiateBlock()
    
    // instantiate the class type
    typedClsDef.tpe = instantiateClassTypeVariable(clsTvar)
    
    typedClsDef.tree.members = typedMemDefs
    
    // add class definition into scope
    scopeCtx.addSymbol(typedClsDef.tree.sym)
    
    // --- relocate to the old scope ---
    scopeCtx.relocateScope()
    relocateTypingScope()
    
    typedClsDef
  }

  /** Type member definitions.
    * 
    * Simply compute the type of the body. Will not check whether ascription and actual type matchs (this will be done
    * within [[Typer.typedClassDef]]. Will not add symbol into the scope.
    */
  def typedMemberDef(memberDef: untpd.MemberDef): tpd.MemberDef = recordTyped {
    val memDef: Trees.MemberDef[Untyped] = memberDef.tree
    
    val body: tpd.Expr = typedExpr(memDef.body)
    val tpe: Type = body.tpe
    
    memDef.assignType(tpe, null, body)
  }


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
      case x : Trees.BlockExpr[Untyped] => typedRecBlockExpr(Untyped(x))
      case x : Trees.ApplyExpr[Untyped] => typedApplyExpr(Untyped(x))
      case x : Trees.IfExpr[Untyped] => typedIfExpr(Untyped(x))
      case _ => throw TypeError(s"can not type $expr : lacking implementation")
    }
  }

  /** Type lambda expressions.
    *
    * ```text
    * Γ, x1 : T1, ..., xn : Tn |- t : T | C
    * ----------------------------------------------------------
    * Γ |- (x1 : T1, ..., xn : Tn) => t : (T1, ..., Tn) => T | C
    * ```
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

    val defs: List[tpd.LocalDef] = block.defs map { d => typedLocalDef(d, recursiveMode = false) }
    val tpdExpr: tpd.Expr = typedExpr(block.expr)

    // --- Close the scope ---
    scopeCtx.relocateScope()

    block.assignType(tpdExpr.tpe, defs, tpdExpr)
  }

  /** Type block expression with recursive definition group.
    *
    * ```text
    * Γ, x1 : X1, ..., xn : Xn |- t1 : T1 | C1
    * ...
    * Γ, x1 : X1, ..., xn : Xn |- tn : Tn | Cn
    * Γ, x1 : X1, ..., xn : Xn |- t : T | C
    * C' = C \/ C1 \/ ... \/ Cn \/ { X1 = T1, ..., Xn = Tn }
    * Xi is the ascripted type of xi if exists, a fresh type
    * variable otherwise
    * ------------------------------------------------------
    * Γ |- { val x1 = t1; ...; val xn = tn; t } : T | C'
    * ```
    */
  def typedRecBlockExpr(expr: untpd.BlockExpr): tpd.BlockExpr = {
    val block: Trees.BlockExpr[Untyped] = expr.tree

    // --- locate a new scope ---
    scopeCtx.locateScope()
    locateTypingScope()
    val untpdDefs: List[Trees.LocalDef[Untyped]] = block.defs map (_.tree)
    var placeholders: Map[String, Symbol[tpd.LocalDefBind]] = Map.empty

    untpdDefs foreach {
      case Trees.LocalDef.Bind(Symbol(symName, _), mutable, ascription, _) =>
        // type of the binding
        val tpe : Type = ascription getOrElse freshXvar
        val tpdBind: tpd.LocalDefBind = Typed(tpe = tpe, tree = Trees.LocalDef.Bind(null, mutable, null, null))
        val sym: Symbol[tpd.LocalDefBind] = Symbol(name = symName, dealias = tpdBind)
        scopeCtx.addSymbol(sym)
        placeholders = placeholders.updated(symName, sym)
      case _ =>
    }

    val tpdDefs: List[tpd.LocalDef] = block.defs map { d => typedLocalDef(d, recursiveMode = true) }

    tpdDefs foreach {
      case Typed(bind : Trees.LocalDef.Bind[Typed], actualType) =>
        val symName = bind.sym.name
        placeholders.get(symName) match {
          case None =>
          case Some(wrapper) =>
            val assumedType = wrapper.dealias.tpe
            wrapper.dealias = Typed(tpe = actualType, tree = bind)
            recordEquality(assumedType, actualType)
        }
      case _ =>
    }

    // instantiate all type variables in the block
    // if the instantiation fails, the recursive definitions types can not be inferred
    forceInstantiateBlock()

    val tpdExpr: tpd.Expr = typedExpr(block.expr)

    // --- relocate to previous scope ---
    scopeCtx.relocateScope()
    relocateTypingScope()

    forceInstantiate { block.assignType(tpdExpr.tpe, tpdDefs, tpdExpr) }
  }

  def typedLocalDef(expr: untpd.LocalDef, recursiveMode: Boolean = true): tpd.LocalDef = recordTyped {
    val localDef: Trees.LocalDef[Untyped] = expr.tree

    localDef match {
      case bind @ Trees.LocalDef.Bind(_, _, tpe, body) =>
        val typedBody = typedExpr(body)
        tpe match {
          case Some(tpe) if !recursiveMode && tpe != typedBody.tpe =>
            throw TypeError(s"Type mismatch: expecting $tpe but found ${typedBody.tpe}")
          case _ => ()
        }
        val typedBind: tpd.LocalDefBind = bind.assignTypeBind(typedBody)
        if !recursiveMode then
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
            if !recursiveMode then
              throw TypeError(s"can not assign value of ${tpdExpr.tpe} to $sym")
            else
              recordEquality(tpdExpr.tpe, typeOfSymbol(sym))
              assign.assignTypeAssign(sym, tpdExpr)
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

  /** Type identifiers.
    *
    * ```
    * x : T ∈ Γ
    * ----------
    * Γ |- x : T
    * ```
    */
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

  /** Type application expression.
    *
    * ```text
    * Γ |- t1 : T1 | C1
    * Γ |- t2 : T2 | C2
    * X is a fresh type variable
    * C' = C1 /\ C2 /\ { T1 = T2 -> X }
    * ---------------------------------
    *        Γ |- t1 t2 : X | C'
    * ```
    */
  def typedApplyExpr(expr: untpd.ApplyExpr): tpd.ApplyExpr = {
    def apply = expr.tree
    val func: tpd.Expr = typedExpr(apply.func)
    val params: List[tpd.Expr] = apply.args map typedExpr
    val paramTypes: List[Type] = params map (_.tpe)

    func.tpe match {
      case LambdaType(expectParamTypes, valueType) =>
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
      case tpe : Type =>
        val retType = freshTvar
        val expectType = LambdaType(paramTypes, retType)
        recordEquality(tpe, expectType)
        apply.assignType(tpe = retType, func, params)
    }
  }

  /** Type if expressions.
    *
    * ```
    * Γ |- t1 : T1 | C1, t2 : T2 | C2, t3 : T3 | C3
    * C' = C1 \/ C2 \/ C3 \/ { T1 = Boolean, T2 = T3 }
    * ------------------------------------------------
    *      Γ |- if t1 then t2 else t3 : T2 | C'
    * ```
    */
  def typedIfExpr(expr: untpd.IfExpr): tpd.IfExpr = {
    val ifExpr: Trees.IfExpr[Untyped] = expr.tree
    val tpdCond: tpd.Expr = typedExpr(ifExpr.cond)
    val tpdTBody: tpd.Expr = typedExpr(ifExpr.trueBody)
    val tpdFBody: tpd.Expr = typedExpr(ifExpr.falseBody)

    val condType: Type = tryInstantiateType(tpdCond.tpe) match {
      case None =>
        recordEquality(tpdCond.tpe, GroundType.BooleanType)
        tpdCond.tpe
      case Some(tpe) if tpe == GroundType.BooleanType =>
        tpe
      case Some(tpe) =>
        throw TypeError(s"expect Boolean in if condition, but found $tpe")
    }

    val (tpeT, tpeF) = (tryInstantiateType(tpdTBody.tpe), tryInstantiateType(tpdFBody.tpe)) match {
      case (Some(tpe1), Some(tpe2)) if tpe1 == tpe2 => (tpe1, tpe2)
      case (Some(tpe1), Some(tpe2)) =>
        throw TypeError(s"body of if expressions have different types: $tpe1 and $tpe2")
      case (_, _) =>
        val (tpe1, tpe2) = (tpdTBody.tpe, tpdFBody.tpe)
        recordEquality(tpe1, tpe2)
        (tpe1, tpe2)
    }

    // instantiate condition and body types if possible
    tpdCond.tpe = condType
    tpdTBody.tpe = tpeT
    tpdFBody.tpe = tpeF

    ifExpr.assignType(tpeT, tpdCond, tpdTBody, tpdFBody)
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
      else if e1 == this.e1 then {
        recordEquality(e2, this.e2)
        Some(res)
      }
      else if e2 == this.e2 then {
        recordEquality(e1, this.e1)
        Some(res)
      } else {
        None
      }
  }

  /** Signature for unary operators.
    */
  trait UnaryOpSig {
    def infer(e: Type): Option[Type]
  }

  case class ConcreteUnaryOpSig(e: Type, res: Type, ambiguous: Boolean = true) extends UnaryOpSig {
    override def infer(e: Type): Option[Type] =
      tryInstantiateType(e) match {
        case None if !ambiguous =>
          recordEquality(e, this.e)
          Some(res)
        case Some(tpe) if tpe == this.e =>
          Some(res)
        case _ =>
          None
      }
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
          else {
            recordEquality(e1, e2)
            Some(BooleanType)
          }
      }
    ),
    bop.!= -> List(
      new BinOpSig {
        override def infer(e1: Type, e2: Type): Option[Type] =
          if e1 == e2 then
            Some(BooleanType)
          else {
            recordEquality(e1, e2)
            Some(BooleanType)
          }
      }
    ),
  )

  /** Signature for unary operators.
    */
  val unaryOpSig: Map[uop, List[UnaryOpSig]] = Map(
    uop.! -> List(
      ConcreteUnaryOpSig(BooleanType, BooleanType, ambiguous = false)
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

package fs2c.ast.c

import fs2c.codegen.{ CodeBundles => bd }
import fs2c.ast.Symbol
import Symbol._

/** Abstract syntax trees for C language.
  */
object Trees {

  /** Types in C language.
    */
  trait Type

  /** Base types in C language.
    */
  enum BaseType extends Type {
    case IntType
    case DoubleType
    case CharType
  }

  /** Function types in C.
    *
    * @param retType Return type of the function. `None` means void.
    * @param paramTypes Parameter types.
    */
  case class FuncType(retType: Type, paramTypes: List[Type]) extends Type

  /** A user-defined struct type in C.
    *
    * @param structSym Associated struct definition symbol.
    */
  case class StructType(structSym: Symbol[StructDef]) extends Type {
    def name: String = structSym.name
    def members: List[StructMember] = structSym.dealias.members
  }

  /** A user-defined alias type.
    */
  case class AliasType(aliasSym: Symbol[TypeAliasDef]) extends Type {
    def dealias: Type = aliasSym.dealias.dealias
  }

  case class PointerType(dealiasType: Type) extends Type

  case object VoidType extends Type

  /** Expressions in C.
    */
  trait Expr

  /** Binary expressions.
    */
  case class BinOpExpr(op: BinOpType, e1: Expr, e2: Expr) extends Expr {
    override def toString: String = s"($op $e1 $e2)"
  }

  enum BinOpType {
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

  case class UnaryOpExpr(op: UnaryOpType, e: Expr) extends Expr

  enum UnaryOpType {
    case -
    case !
  }

  /** Calling a function.
    *
    * @param funcSym Symbol of the function to be called.
    */
  case class CallFunc(func: Expr, params: List[Expr]) extends Expr
  
  case class SizeOf(tp: Type) extends Expr

  /** Functions in C language.
    *
    * Can either be a [[GroundFunc]] or a [[FuncDef]].
    */
  trait Func {
    def appliedTo(es: List[Expr]): Expr = this match {
      case g : GroundFunc => CallFunc(g, es.toList)
      case f : FuncDef => CallFunc(IdentifierExpr(f.sym), es.toList)
    }
    def appliedTo(es: Expr*): Expr = appliedTo(es.toList)
    def $$ (es: Expr*): Expr = appliedTo(es: _*)
  }

  case class GroundFunc(name: String, header: List[String]) extends Func, Expr


  /** Literal expression.
    */
  trait LiteralExpr

  /** Int expression.
    */
  case class IntExpr(value: Int) extends Expr, LiteralExpr {
    override def toString: String = s"$value"
  }

  extension (i: Int) {
    def asC: Expr = IntExpr(i)
  }

  /** Float expression.
    */
  case class FloatExpr(value: Double) extends Expr, LiteralExpr {
    override def toString: String = s"$value"
  }

  extension (i: Float) {
    def asC: Expr = FloatExpr(i)
  }

  /** Boolean expression.
    */
  case class BoolExpr(value: Boolean) extends Expr, LiteralExpr {
    override def toString: String = s"$value"
  }

  extension (i: Boolean) {
    def asC: Expr = BoolExpr(i)
  }

  case class StringExpr(value: String) extends Expr, LiteralExpr {
    override def toString: String = s"\"$value\""
  }

  extension (s: String) {
    def asC: Expr = StringExpr(s)
  }

  /** Identifier expression referring a C [[Symbol]].
    */
  case class IdentifierExpr[T](sym: Symbol[T]) extends Expr {
    def addressExpr: AddressExpr[T] = AddressExpr(this)
  }

  extension[T] (sym: Symbol[T]) {
    def cIdent: IdentifierExpr[T] = IdentifierExpr(sym)
  }

  case class AddressExpr[T](e: IdentifierExpr[T]) extends Expr

  /** Select expression that selects a member from a C struct.
    * 
    * @param expr The expression to be selected.
    * @param designator The symbol of the member to select.
    */
  case class SelectExpr(expr: Expr, designator: Symbol[StructMember]) extends Expr

  /** Coercion expression in C.
    */
  case class CoercionExpr(tp: Type, expr: Expr) extends Expr

  /** Definitions in the C language.
    */
  sealed trait Definition

  /** Function definition in C.
    * 
    * @param sym The bound symbol of the function.
    * @param _retType
    * @param params
    * @param block
    */
  case class FuncDef(sym: Symbol[FuncDef], _retType: Type, params: List[FuncParam], block: Block) extends Definition, Func
  
  object FuncDef {
    def makeFuncDef(name: String, retType: Type, params: List[FuncParam], body: Block): FuncDef = {
      val res = FuncDef(Symbol(name, null), retType, params, body)
      res.sym.dealias = res

      res
    }
  }
  
  sealed trait Binding {
    type ThisType <: Binding
    def getSym: Symbol[ThisType]
    def getType: Type
  }

  case class FuncParam(sym: Symbol[FuncParam], tp: Type) extends Binding {
    override type ThisType = FuncParam

    override def getSym = sym
    
    override def getType = tp
  }
  
  object FuncParam {
    def makeFuncParam(name: String, tp: Type): FuncParam = {
      val res = FuncParam(Symbol(name, null), tp)
      res.sym.dealias = res
      
      res
    }
  }

  type Block = List[Statement]

  enum Statement {
    case Return(expr: Option[Expr])
    case Break
    case Continue
    case If(cond: Expr, thenBody: Block, elseBody: Option[Block])
    case While(cond: Expr, body: Block)
    case Eval(e: Expr)
    case AssignVar(d: Binding, expr: Expr)
    case AssignMember(v: Binding, d: Symbol[StructMember], expr: Expr)
    case Assign(p: Expr, expr: Expr)
    case Def(d: VariableDef)
  }

  case class TypeAliasDef(sym: Symbol[TypeAliasDef], dealias: Type) extends Definition
  
  object TypeAliasDef {
    def makeTypeAliasDef(name: String, dealias: Type): TypeAliasDef = {
      val d: TypeAliasDef = TypeAliasDef(Symbol(name, null), dealias)
      d.sym.dealias = d
      d
    }
  }

  case class StructDef(sym: Symbol[StructDef], members: List[StructMember]) extends Definition {
    def tp: StructType = StructType(sym)
    
    def ensureFind(name: String): StructMember = members find { m => m.sym.name == name } match {
      case None =>
        assert(false, s"can not find $name")
      case Some(m) => m
    }
  }

  object StructDef {
    def makeStructDef(name: String, memberDefs: List[(String, Type)]): StructDef = {
      val structSym: Symbol[StructDef] = Symbol(name = name, dealias = null)
      val members = memberDefs map { (name, t) => StructMember.makeStructMember(name, t, structSym) }
      val struct = StructDef(structSym, members)
      structSym.dealias = struct

      struct
    }
  }

  case class StructMember(sym: Symbol[StructMember], tp: Type, var struct: Symbol[StructDef])
  
  object StructMember {
    def makeStructMember(name: String, tp: Type, structSym: Symbol[StructDef]) = {
      val m = StructMember(Symbol[StructMember](name, null), tp, structSym)
      m.sym.dealias = m
      m
    }
  }

  case class VariableDef(sym: Symbol[VariableDef], tp: Type, expr: Option[Expr] = None, var associatedBundle: Option[bd.ValueBundle] = None) extends Binding {
    override type ThisType = VariableDef

    override def getSym = sym

    override def getType = tp
  }

  object VariableDef {
    def makeVariableDef(name: String, tp: Type, expr: Option[Expr] = None): VariableDef = {
      val sym: Symbol[VariableDef] = Symbol(name, null)
      val d = VariableDef(sym, tp, expr)
      sym.dealias = d
      d
    }
  }

  extension (name : String) {
    def :: (tp: Type): VariableDef = VariableDef.makeVariableDef(name, tp)
  }
}

package fs2c.typer

import fs2c.io.Positional
import fs2c.ast.Symbol
import fs2c.codegen.{ CodeBundles => bd }
import fs2c.ast.fs.Trees.{tpd, untpd}

object Types {
  /** The trait for all types.
    */
  trait Type {
    var code: bd.CodeBundle = bd.NoCode

    def assignCode[T <: bd.CodeBundle](bundle: => T): T = {
      val gen = bundle
      code = gen
      gen
    }

    var isRef: Boolean = false

    def refType: this.type =
      isRef = true
      this

    def refTypeWhen(cond: => Boolean): this.type =
      if cond then refType else this
  }

  /** A proxy type referring to type through a symbol.
    */
  case class SymbolType(refSym: Symbol.Ref) extends Type {
    override def toString: String = refSym.name
  }

  /** Ground types.
    */
  enum GroundType extends Type {
    case UnitType
    case IntType
    case FloatType
    case BooleanType
    case StringType
    case ArrayType(itemType: Type)

    override def toString: String = this match {
      case UnitType => "Unit"
      case IntType => "Int"
      case FloatType => "Float"
      case BooleanType => "Boolean"
      case StringType => "String"
      case ArrayType(itemType) => s"Array[$itemType]"
    }
  }

  /** Lambda types.
    */
  case class LambdaType(paramTypes: List[Type], valueType: Type) extends Type {
    override def toString: String = s"(${paramTypes.map(_.toString).mkString(", ")}) => $valueType"
  }

  /** Predicates on type variable.
    * 
    * Carries additional information about the shape of the type variable.
    */
  enum Predicate {
    /** Asserts that X has a `member` of type `tpe`.
      * 
      * ```scala
      * X <: { member : tpe }
      * ```
      */
    case HaveMemberOfType(member: String, tpe: Type)
  }
  /** Type variables.
    */
  case class TypeVariable(name: String, var predicates: List[Predicate]) extends Type with Positional {
    type PosSelf = TypeVariable

    def mergePredicates(tv: TypeVariable): Unit =
      this.predicates = tv.predicates ++ this.predicates
    
    def occursIn(tpe: Type): Boolean = tpe match {
      case tv : TypeVariable => tv == this
      case GroundType.ArrayType(itemTpe) => occursIn(itemTpe)
      case LambdaType(tpes, valTpe) =>
        (valTpe :: tpes) exists { occursIn(_) }
      case _ => false
    }

    override def toString: String = name
  }

  /** Special type variable recording information about the *shape* of a class type definition.
    */
  case class ClassTypeVariable(classDef: tpd.ClassDef, var predicates: List[Predicate]) extends Type {
    import Predicate._

    override def toString: String = s"ClassTypeVar(${classDef.tree})"
  }
}

package fs2c.typer

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
  }

  /** A proxy type referring to type through a symbol.
    */
  case class SymbolType(refSym: Symbol.Ref) extends Type

  /** Ground types.
    */
  enum GroundType extends Type {
    case UnitType
    case IntType
    case FloatType
    case BooleanType
    case StringType
    case ArrayType(itemType: Type)
  }

  /** Lambda types.
    */
  case class LambdaType(paramTypes: List[Type], valueType: Type) extends Type

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
  case class TypeVariable(name: String, var predicates: List[Predicate]) extends Type {
    def mergePredicates(tv: TypeVariable): Unit =
      this.predicates = tv.predicates ++ this.predicates
    
    def occursIn(tpe: Type): Boolean = tpe match {
      case tv : TypeVariable => tv == this
      case GroundType.ArrayType(itemTpe) => occursIn(itemTpe)
      case LambdaType(tpes, valTpe) =>
        (valTpe :: tpes) exists { occursIn(_) }
      case _ => false
    }
  }

  /** Special type variable recording information about the *shape* of a class type definition.
    */
  case class ClassTypeVariable(classDef: tpd.ClassDef, var predicates: List[Predicate]) extends Type {
    import Predicate._

    override def toString: String = s"ClassTypeVar(${classDef.tree})"
  }
}

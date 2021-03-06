package fs2c.typer

import fs2c.ast.Symbol

trait Types {
  /** The trait for all types.
    */
  trait Type

  /** A proxy type referring to type through a symbol.
    */
  case class SymbolType(refSym: Symbol.Ref) extends Type

  /** Ground types.
    */
  enum GroundType extends Type {
    case IntType
    case FloatType
    case BooleanType
    case StringType
    case ArrayType(itemType: Type)
  }

  /** Lambda types.
    */
  case class LambdaType(paramTypes: List[Type], valueType: Type) extends Type
}

object Types extends Types

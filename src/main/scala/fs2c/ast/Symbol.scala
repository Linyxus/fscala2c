package fs2c.ast

/** A symbol with `name` and reference to a value of `T`.
 */
case class Symbol[T](name: String, ref: T)

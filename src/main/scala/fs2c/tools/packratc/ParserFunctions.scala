package fs2c.tools.packratc

import fs2c.tools.packratc.Parser.{ParseError, Result}

trait ParserFunctions {
  /** Simply returns the `value`. No parsing.
   */
  def pure[T, X](value: X) = new Parser[T, X](None) {
    override def _parse(xs: LazyList[T])(using ctx: ParserContext[T]): Result[T, X] = Right(value, xs)
  }
  
  /** Parse a token that satisifes the predicate.
   */
  def satisfy[T](predicate: T => Boolean, desc: Option[String] = None) = new Parser[T, T](desc) {
    override def _parse(xs: LazyList[T])(using ctx: ParserContext[T]): Result[T, T] = xs match {
      case x #:: xs if predicate(x) => Right(x, xs)
      case x #:: _ => fail(xs, Nil)
      case _ => fail(xs, Nil)
    }
  }
  
  /** Make a parser that matches the token `expected`.
   */
  def token[T](expected: T) = new Parser[T, T](Some(s"token $expected")) {
    override def _parse(xs: LazyList[T])(using ctx: ParserContext[T]): Result[T, T] = xs match {
      case x #:: xs if x == expected => Right(expected, xs)
      case x #:: _ => fail(xs, Nil)
      case _ => fail(xs, Nil)
    }
  } is s"$expected"

  /** Parse nothing, simply masking the start of a block and pushing the current indent level into the stack.
   */
  def blockStart[T] = new Parser[T, Unit](None) { // should never fail
    override protected def _parse(xs: LazyList[T])(using ctx: ParserContext[T]): Result[T, Unit] =
      Right(ctx.stream.startBlock, xs)
  }

  /** Parse nothing. Pop the indent level from the stack.
   */
  def blockEnd[T] = new Parser[T, Unit](None) { // should never fail
    override protected def _parse(xs: LazyList[T])(using ctx: ParserContext[T]): Result[T, Unit] =
      Right(ctx.stream.endBlock, xs)
  }
}

object ParserFunctions extends ParserFunctions

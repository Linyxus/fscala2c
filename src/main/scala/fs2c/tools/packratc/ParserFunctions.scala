package fs2c.tools.packratc

import fs2c.tools.packratc.Parser.{ParserError, Result}

trait ParserFunctions {
  /** Simply returns the `value`. No parsing.
   */
  def pure[T, X](value: X) = new Parser[T, X] {
    override def _parse(xs: LazyList[T])(using ctx: ParserContext[T]): Result[T, X] = Right(value, xs)
  }
  
  /** Parse a token that satisifes the predicate.
   */
  def satisfy[T](predicate: T => Boolean) = new Parser[T, T] {
    override def _parse(xs: LazyList[T])(using ctx: ParserContext[T]): Result[T, T] = xs match {
      case x #:: xs if predicate(x) => Right(x, xs)
      case x #:: _ => Left(ParserError(Some(x), s"do not satisfy the predicate"))
      case _ => Left(ParserError(None, s"expect more token, but find end of stream"))
    }
  }
  
  /** Make a parser that matches the token `expected`.
   */
  def token[T](expected: T) = new Parser[T, T] {
    override def _parse(xs: LazyList[T])(using ctx: ParserContext[T]): Result[T, T] = xs match {
      case x #:: xs if x == expected => Right(expected, xs)
      case x #:: _ => Left(ParserError(Some(x), s"expected: $expected"))
      case _ => Left(ParserError(None, s"expected: $expected, but find end of stream"))
    }
  } is s"$expected"

  /** Parse nothing, simply masking the start of a block and pushing the current indent level into the stack.
   */
  def blockStart[T] = new Parser[T, Unit] {
    override protected def _parse(xs: LazyList[T])(using ctx: ParserContext[T]): Result[T, Unit] =
      Right(ctx.stream.startBlock, xs)
  }

  /** Parse nothing. Pop the indent level from the stack.
   */
  def blockEnd[T] = new Parser[T, Unit] {
    override protected def _parse(xs: LazyList[T])(using ctx: ParserContext[T]): Result[T, Unit] =
      Right(ctx.stream.endBlock, xs)
  }
}

object ParserFunctions extends ParserFunctions

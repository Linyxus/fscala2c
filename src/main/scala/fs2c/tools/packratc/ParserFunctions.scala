package fs2c.tools.packratc

import fs2c.tools.packratc.Parser.{ParseError, Result}
import fs2c.tools.packratc.Parser.~

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

  def choice[T, X](parsers: List[Parser[T, X]]): Parser[T, X] = {
    assert(parsers.nonEmpty, "can not make choice over an empty list of parsers")
    parsers.tail.foldLeft(parsers.head) { (acc, x) => acc or x }
  }

  object ExpressionParser {

    /** Associativity of operators.
      */
    enum OpAssoc {
      case LeftAssoc
      case RightAssoc
    }

    /** Infomation of operator.
      * [[OpType.Unary]] means prefix operators; [[OpType.Binary]] means infix operators.
      */
    enum OpInfo[T, X] {
      case Unary(ops: List[Parser[T, X => X]])
      case Binary(assoc: OpAssoc, ops: List[Parser[T, (X, X) => X]])
    }

    /** Table of operators in the expression grammar.
      *
      * @tparam T Input token type.
      * @tparam X Expression type.
      */
    type OpTable[T, X] = List[OpInfo[T, X]]

    /** Generate a expression parser from the given operator table.
      *
      * @param table Operator table for the grammar.
      * @param term Parser for term in the grammar.
      */
    def makeExprParser[T, A, X](table: OpTable[T, X], term: Parser[T, X]): Parser[T, X] = {
      import OpInfo._
      import OpAssoc._

      def recur(rows: OpTable[T, X], p: Parser[T, X]): Parser[T, X] = rows match {
        case Nil => p
        case Unary(ops) :: rows =>
          val q = (choice(ops).optional seq p) map {
            case None ~ r => r
            case Some(fun) ~ r =>  fun(r)
          }
          recur(rows, q)
        case Binary(assoc, ops) :: rows =>
          val q = (p seq (choice(ops) seq p).many) map { case x ~ ys =>
            val xs: List[((X, X) => X) ~ X] = ys

            assoc match {
              case LeftAssoc =>
                xs.foldLeft(x) { case (acc, op ~ x) => op(acc, x) }
              case RightAssoc =>
                val fun: X => X = xs.foldRight(identity[X]) { case (op ~ x, acc) => l => op(l, acc(x)) }
                fun(x)
            }
          }
          recur(rows, q)
      }

      recur(table.reverse, term)
    }
  }
}

object ParserFunctions extends ParserFunctions

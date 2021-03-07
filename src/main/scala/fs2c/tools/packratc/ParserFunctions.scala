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
    enum OpInfo[T, A, X] {
      case Unary(op: Parser[T, A], builder: X => X)
      case Binary(assoc: OpAssoc, op: Parser[T, A], builder: (X, X) => X)
    }

    /** Table of operators in the expression grammar.
      * 
      * @tparam T Input token type.
      * @tparam A Operator type.
      * @tparam X Expression type.
      */
    type OpTable[T, A, X] = List[OpInfo[T, A, X]]

    /** Generate a expression parser from the given operator table.
      * 
      * @param table Operator table for the grammar.
      * @param term Parser for term in the grammar.
      */
    def makeExprParser[T, A, X](table: OpTable[T, A, X], term: Parser[T, X]): Parser[T, X] = {
      import OpInfo._
      import OpAssoc._
      
      def recur(rows: OpTable[T, A, X], p: Parser[T, X]): Parser[T, X] = rows match {
        case Nil => p
        case Unary(op, builder) :: rows =>
          val q = (op.optional seq p) map {
            case None ~ r => r
            case Some(x) ~ r => builder(r)
          }
          recur(rows, q)
        case Binary(assoc, op, builder) :: rows =>
          val q = (p seq (op seq p).many) map { case x ~ ys =>
            val xs: List[X] = x :: ys.map { case _ ~ x => x }
            
            assoc match {
              case LeftAssoc =>
                xs.tail.foldLeft(xs.head)(builder)
              case RightAssoc =>
                xs.init.foldRight(xs.last)(builder)
            }
          }
          recur(rows, q)
      }
      
      recur(table.reverse, term)
    }
  }
}

object ParserFunctions extends ParserFunctions

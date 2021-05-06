package fs2c.tools.packratc

import fs2c.tools.packratc.Parser.{ParseError, Result}
import fs2c.io.Positional

/** A Packrat parser for PEG, parsing a stream of tokens of type T and producing a value of type X.
  * ```
  * LazyList[Token] --- Parser[Token, Value] --> Value
  * ```
  * @tparam T The input token type.
  * @tparam X The output value type.
  */
abstract class Parser[T, X](val desc: Option[String]) {

  import Parser.~

  protected var cache: Map[LazyList[_], Parser.Result[T, X]] = Map.empty

  /** Run the parser on the given input with a context. The results are memoized.
    *
    * @param xs  The input token stream.
    * @param ctx The parsing context.
    * @return The result of parsing.
    */
  def parse(xs: LazyList[T])(using ctx: ParserContext[T]): Parser.Result[T, X] = {
//    cache.get(xs) match {
//      case Some(value) =>
//        value
//      case None =>
//        val result = _parse(xs)
//        cache = cache.updated(xs, result)
//        result
//    }
    _parse(xs)
  }

  /** The actual parsing logic. Do not take care of memoization.
    *
    * @param xs
    * @param ctx
    * @return
    */
  protected def _parse(xs: LazyList[T])(using ctx: ParserContext[T]): Parser.Result[T, X]

  private def headToken(xs: LazyList[T]): Option[T] = xs match {
    case t #:: _ => Some(t)
    case _ => None
  }

  /** Return the same parser as `this` except for a different description string.
    * The `_parse` logic and the packrat `cache` will be copied.
    */
  def is(whatStr: String): Parser[T, X] = {
    val that = this
    new Parser[T, X](Some(whatStr)) {
      override protected def _parse(xs: LazyList[T])(using ctx: ParserContext[T]): Result[T, X] =
        that._parse(xs) match {
          case Left(_) => Left(ParseError(whatStr))
          case x => x
        }

      cache = that.cache
    }
  }

  /** Parsers are functors.
    */
  def map[Y](func: X => Y): Parser[T, Y] = {
    def newParse(xs: LazyList[T])(using ctx: ParserContext[T]): Parser.Result[T, Y] = {
      parse(xs) map { case (x, s) => (func(x), s) }
    }

    new Parser[T, Y](desc) {
      override def _parse(xs: LazyList[T])(using ctx: ParserContext[T]): Result[T, Y] = newParse(xs)
    }
  }

  /** Parsers are monads.
    */
  def flatMap[Y](mfunc: X => Parser[T, Y]) = {
    def newParse(xs: LazyList[T])(using ctx: ParserContext[T]): Parser.Result[T, Y] = {
      parse(xs) flatMap { case (x, s) =>
        val other = mfunc(x)
        other.parse(s)
      }
    }

    new Parser[T, Y](desc) {
      override def _parse(xs: LazyList[T])(using ctx: ParserContext[T]): Result[T, Y] = newParse(xs)
    }
  }

  /** Producing a new parser parsing two parsers `this` and `other` in sequence.
    *
    * `p1 seq p2` succeeds only if `p1` succeeds and `p2` also succeeds on the remaining part of the input.
    *
    * @param other Parser to run after `this`.
    * @tparam Y Result value type of `other`.
    * @return A new parser constructed.
    */
  def seq[Y](other: => Parser[T, Y]): Parser[T, X ~ Y] = {
    val that = this

    new Parser[T, X ~ Y](desc map { s => s"$s" }) {
      override def _parse(xs: LazyList[T])(using ctx: ParserContext[T]): Result[T, X ~ Y] = {
        that.parse(xs) match {
          case Left(_) => fail
          case Right(x, rem) =>
            other.parse(rem) match {
              case Left(_) => fail
              case Right(y, rem) => Right(new ~(x, y), rem)
            }
        }
      }
    }
  }

  /** Producing a new parser parsing two parsers either `this` or `other`.
    *
    * `p1 choice p2` fails only if `p1` fails and `p2` also fails on the *original* input.
    *
    * @param other Parser to run after `this` fails.
    * @tparam Y Result value type of `other`.
    * @return A new parser constructed.
    */
  def or[Y](other: => Parser[T, Y]): Parser[T, X | Y] = {
    val that = this

    new Parser[T, X | Y](desc map { s => s"$s" }) {
      override def _parse(xs: LazyList[T])(using ctx: ParserContext[T]): Result[T, X | Y] =
        that.parse(xs) match {
          case Right(x, rem) => Right(x, rem)
          case Left(_) => other.parse(xs) match {
            case Right(x, rem) => Right(x, rem)
            case Left(ParseError(_)) => fail
          }
        }
    }
  }

  /** Returns a parser that parses the input optionally.
    */
  def optional: Parser[T, Option[X]] = {
    def newParse(xs: LazyList[T])(using ctx: ParserContext[T]): Parser.Result[T, Option[X]] = {
      parse(xs) match {
        case Right(x, rem) => Right(Some(x), rem)
        case Left(_) => Right(None, xs)
      }
    }

    new Parser[T, Option[X]](desc) {
      override def _parse(xs: LazyList[T])(using ctx: ParserContext[T]): Result[T, Option[X]] = newParse(xs)
    }
  }

  /** Returns a new parser that parses `this` for zero or more times.
    */
  def many: Parser[T, List[X]] = {
    val whatStr = desc map { s => s"zero or more $s" }

    def newParse(xs: LazyList[T])(using ctx: ParserContext[T]): Parser.Result[T, List[X]] = {
      @annotation.tailrec def recur(xs: LazyList[T], acc: List[X]): (List[X], LazyList[T]) =
        parse(xs) match {
          case Left(_) => (acc, xs)
          case Right(value, rem) => recur(rem, value :: acc)
        }

      Right(recur(xs, Nil) match { case (xs, rem) => (xs.reverse, rem) })
    }

    new Parser[T, List[X]](whatStr) {
      override def _parse(xs: LazyList[T])(using ctx: ParserContext[T]): Result[T, List[X]] = newParse(xs)
    }
  }

  /** Returns a new parser that parses `this` for one or more times.
    */
  def some: Parser[T, X ~ List[X]] =
    this seq many

  /** Returns a parser the same as `this`, except that it will not consume input even if it succeeds.
    */
  def lookAhead: Parser[T, X] = {
    def newParse(xs: LazyList[T])(using ctx: ParserContext[T]): Parser.Result[T, X] = {
      parse(xs) match {
        case Left(e) => Left(e)
        case Right(value, rem) => Right(value, xs)
      }
    }

    new Parser[T, X](desc) {
      override def _parse(xs: LazyList[T])(using ctx: ParserContext[T]): Result[T, X] = newParse(xs)
    }
  }

  /** A parser that succeeds iff `this` fails.
    */
  def not: Parser[T, Unit] = {
    val errorMsg = desc map { s => s"something not $s" }
    val that = this

    new Parser[T, Unit](errorMsg) {
      override def _parse(xs: LazyList[T])(using ctx: ParserContext[T]): Result[T, Unit] =
        that.parse(xs) match {
          case Left(e) => Right((), xs)
          case Right(value, rem) => fail
        }
    }
  }

  protected def fail[Y]: Result[T, Y] = Left {
    ParseError(
      desc match {
        case None => "<unknown>"
        case Some(s) => s
      }
    )
  }
}

object Parser extends ParserFunctions {

  /** Records the error throwed by a parser.
    *
    * @param token Token where the error is generated. Optional.
    * @param msg   Error message.
    * @tparam T Input token type of the related parser (for storing the related token).
    */
  case class ParseError[T](errMsg: String) extends Positional {
    type PosSelf = ParseError[T]

    override def toString: String =
      s"[Parse Error] $errMsg\nat\n${showWithContext(2)}"
  }

  /** Result of parsing.
    *
    *  - `Left(e)` means the parser fails with error e`.
    *  - `Right(x, s)` means the parser succeeds with result value x` and remaining token stream `s`.
    */
  type Result[T, X] = Either[ParseError[T], (X, LazyList[T])]

  case class ~[+A, +B](_1: A, _2: B) {
    override def toString: String = s"($_1 ~ $_2)"
  }

}

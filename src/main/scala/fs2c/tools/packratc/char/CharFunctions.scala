package fs2c.tools.packratc.char

import fs2c.tools.data._

import fs2c.tools.packratc.{ParserFunctions => general}
import CharParser._
import LazyListTools._
import fs2c.tools.packratc.Parser.~

/** Tool functions for char Parser.
  */
trait CharFunctions {
  /** Parse string `input` with `parser`.
    */
  def parseString[X](parser: Parser[X], input: String): Result[X] = {
    val stream = new CharStream {
      override val allTokens: LazyList[Char] = fromList(input.toList)

      override def startBlock: Unit = ()

      override def endBlock: Unit = ()
    }
    val context = new ParserContext(stream)
    parser.parse(stream.allTokens)(using context)
  }

  /** Match next character if it satisfies the predicate.
    */
  def satisfy(predicate: Char => Boolean): Parser[Char] = general.satisfy(predicate)

  /** Match next `char`.
    */
  def char(char: Char): Parser[Char] = satisfy(_ == char)

  /** Parse a string `str`. 
    * Not optimized. DO NOT use in production.
    */
  def string(str: String): Parser[String] = {
    @annotation.tailrec def recur(xs: List[Char], acc: Parser[String]): Parser[String] = xs match {
      case Nil => acc
      case x :: xs => recur(xs, {
        acc ~ char(x)
      } <| { case str ~ c => s"$str$c" })
    }

    recur(str.toList, general.pure(""))
  }
}

object CharFunctions extends CharFunctions

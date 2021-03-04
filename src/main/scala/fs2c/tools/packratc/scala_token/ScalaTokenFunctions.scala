package fs2c.tools.packratc.scala_token

import fs2c.tools.packratc.{ParserFunctions => general}
import fs2c.tokenizer.{ScalaToken, ScalaTokenType, Tokenizer}
import ScalaTokenParser._

/** Tool functions for ScalaToken parser.
 */
trait ScalaTokenFunctions {
  /** Run the parse with the tokenizer.
   */
  def parseWithTokenizer[X](parser: Parser[X], tokenizer: Tokenizer): Result[X] = {
    val stream = new Stream {
      override val allTokens: LazyList[ScalaToken] = tokenizer.allTokensLazy
      override def startBlock: Unit = tokenizer.prepareStartBlock()
      override def endBlock: Unit = tokenizer.endBlock()
    }
    val ctx = new ParserContext(stream)
    parser.parse(stream.allTokens)(using ctx)
  }

  /** Parse a token satisfying the predicate.
   */
  def satisfy(predicate: ScalaToken => Boolean): Parser[ScalaToken] =
    general.satisfy(predicate)

  /** Parse some specific token.
   */
  def token(expected: ScalaToken): Parser[ScalaToken] =
    general.token(expected)
  
  /** Parse an identifier.
   */
  def identifier: Parser[ScalaToken] = satisfy {
    case ScalaToken(_, _, _ : ScalaTokenType.Identifier) => true
    case _ => false
  } ?? "identifier"

  /** Parse a token of token type `expected`.
   */
  def tokenOfType(expected: ScalaTokenType): Parser[ScalaToken] = satisfy {
    case ScalaToken(_, _, tokenType) if tokenType == expected => true
    case _ => false
  } is s"token of type $expected"
}

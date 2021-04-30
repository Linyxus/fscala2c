package fs2c.tools.packratc.scala_token

import fs2c.tools.packratc.{ParserFunctions => general}
import fs2c.tokenizer.{ScalaToken, ScalaTokenType, Tokenizer}
import ScalaTokenParser._
import fs2c.tools.packratc.Parser.~

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
  
  def pure[X](value: X): Parser[X] = general.pure(value)

  /** Parse a token satisfying the predicate.
    */
  def satisfy(predicate: ScalaToken => Boolean, desc: Option[String] = None): Parser[ScalaToken] =
    general.satisfy(predicate, desc = desc)

  /** Parse some specific token.
    */
  def token(expected: ScalaToken): Parser[ScalaToken] =
    general.token(expected)

  /** Parse an identifier.
    */
  def identifier: Parser[ScalaToken] = satisfy {
    case ScalaToken(_: ScalaTokenType.Identifier) => true
    case _ => false
  } ?? "identifier"
  
  def literalInt: Parser[ScalaToken] = satisfy {
    case ScalaToken(_ : ScalaTokenType.LiteralInt) => true
    case _ => false
  } ?? "integer literal"
  
  def literalFloat: Parser[ScalaToken] = satisfy {
    case ScalaToken(_ : ScalaTokenType.LiteralFloat) => true
    case _ => false
  } ?? "float literal"
  
  def literalBoolean: Parser[ScalaToken] = satisfy {
    case ScalaToken(_ : ScalaTokenType.LiteralBoolean) => true
    case _ => false
  } ?? "boolean literal"
  
  def symbol(name: String): Parser[ScalaToken] = satisfy {
    case ScalaToken(ScalaTokenType.Identifier(s)) if s == name => true
    case _ => false
  } ?? s"symbol($name)"

  /** Parse a token of token type `expected`.
    */
  def tokenOfType(expected: ScalaTokenType): Parser[ScalaToken] = satisfy(
    {
      case ScalaToken(tokenType) if tokenType == expected => true
      case _ => false
    },
    desc = Some(s"$expected")
  ) is s"token of type $expected"

  extension[X] (p: Parser[X]) {
    /** Parse a list of `p` seperated by `sep`.
      */
    def sepBy[S](sep: Parser[S]): Parser[List[X]] =
      sepBy1(sep).optional <| {
        case None => Nil
        case Some(xs) => xs
      }

    /** Parse at list one `p` seperated by `sep`.
      */
    def sepBy1[S](sep: Parser[S]): Parser[List[X]] = {
      val q: Parser[X ~ List[S ~ X]] = p ~ (sep ~ p).many
      q <| { case x ~ ys => 
        val xs: List[X] = ys map { case _ ~ x => x }
        x :: xs
      }
    }
    
    def wrappedBy[S](l: Parser[S], r: Parser[S]): Parser[X] = l >> p << r
  }
}

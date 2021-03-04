package fs2c.tools.packratc.scala_token

import fs2c.tools.packratc
import fs2c.tokenizer.ScalaToken
import fs2c.tools.packratc.scala_token.syntax.ParserSyntax

trait ScalaTokenParser {
  type Parser[X] = packratc.Parser[ScalaToken, X]
  type Result[X] = packratc.Parser.Result[ScalaToken, X]
  type Stream = packratc.TokenStream[ScalaToken]
  type ParserContext = packratc.ParserContext[ScalaToken]
}

object ScalaTokenParser extends ScalaTokenParser with packratc.syntax.ParserSyntax with ScalaTokenFunctions with ParserSyntax

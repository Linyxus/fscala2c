package scala2c.tools.packratc.char

import scala2c.tools.packratc

trait CharParser {
  type Parser[X] = packratc.Parser[Char, X]
  type Result[X] = packratc.Parser.Result[Char, X]
  type CharStream = packratc.TokenStream[Char]
  type ParserContext = packratc.ParserContext[Char]
}

object CharParser extends CharParser with packratc.syntax.ParserSyntax with CharFunctions

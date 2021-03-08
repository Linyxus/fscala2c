package fs2c.tools.packratc.scala_token.syntax

import fs2c.tools.packratc.scala_token.ScalaTokenParser._
import fs2c.tools.packratc.scala_token._
import fs2c.tokenizer.{ScalaToken, ScalaTokenType}

trait ParserSyntax {
  case class StringToParserConversionError(msg: String) extends Exception(msg)
  
  given Conversion[String, Parser[ScalaToken]] with
    def apply(str: String): Parser[ScalaToken] = str match {
      case "+" => tokenOfType(ScalaTokenType.Plus)
      case "-" => tokenOfType(ScalaTokenType.Minus)
      case "*" => tokenOfType(ScalaTokenType.Asterisk)
      case "/" => tokenOfType(ScalaTokenType.Slash)
      case "^" => tokenOfType(ScalaTokenType.Caret)
      case "=" => tokenOfType(ScalaTokenType.Equal)
      case ":" => tokenOfType(ScalaTokenType.Colon)
      case ";" => tokenOfType(ScalaTokenType.Semicolon)
      case "." => tokenOfType(ScalaTokenType.Dot)
      case "," => tokenOfType(ScalaTokenType.Comma)
      case "{" => tokenOfType(ScalaTokenType.LeftBrace)
      case "}" => tokenOfType(ScalaTokenType.RightBrace)
      case "[" => tokenOfType(ScalaTokenType.LeftBracket)
      case "]" => tokenOfType(ScalaTokenType.RightBracket)
      case "(" => tokenOfType(ScalaTokenType.LeftParen)
      case ")" => tokenOfType(ScalaTokenType.RightParen)
      case "&&" => tokenOfType(ScalaTokenType.DoubleAmpersand)
      case "||" => tokenOfType(ScalaTokenType.DoubleVerticalBar)
      case ">" => tokenOfType(ScalaTokenType.GreaterThan)
      case "<" => tokenOfType(ScalaTokenType.LessThan)
      case ">=" => tokenOfType(ScalaTokenType.GreaterThanEqual)
      case "<=" => tokenOfType(ScalaTokenType.LessThanEqual)
      case "==" => tokenOfType(ScalaTokenType.EqualEqual)
      case "!=" => tokenOfType(ScalaTokenType.BangEqual)
      case "!" => tokenOfType(ScalaTokenType.Bang)
      case "=>" => tokenOfType(ScalaTokenType.BigRightArrow)
      case "def" => tokenOfType(ScalaTokenType.KeywordDef)
      case "val" => tokenOfType(ScalaTokenType.KeywordVal)
      case "var" => tokenOfType(ScalaTokenType.KeywordVar)
      case "if" => tokenOfType(ScalaTokenType.KeywordIf)
      case "else" => tokenOfType(ScalaTokenType.KeywordElse)
      case "then" => tokenOfType(ScalaTokenType.KeywordThen)
      case "class" => tokenOfType(ScalaTokenType.KeywordClass)
      case "extends" => tokenOfType(ScalaTokenType.KeywordExtends)
      case "new" => tokenOfType(ScalaTokenType.KeywordNew)
      case "while" => tokenOfType(ScalaTokenType.KeywordWhile)
      case "do" => tokenOfType(ScalaTokenType.KeywordDo)
      case s => throw StringToParserConversionError(s"Unknown string pattern: $s")
  }
  
  val EOF = tokenOfType(ScalaTokenType.EndOfSource)
  val NL = tokenOfType(ScalaTokenType.NewLine)
}

object ParserSyntax extends ParserSyntax

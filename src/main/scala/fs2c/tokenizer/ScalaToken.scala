package fs2c.tokenizer

import fs2c.io.SourcePos

enum ScalaTokenType {

  case LeftBrace

  case RightBrace

  case LeftBracket

  case RightBracket

  case LeftParen

  case RightParen

  case KeywordVal

  case KeywordVar

  case KeywordDef

  case Equal

  case Colon

  case Comma

  case Dot

  case BigRightArrow

  case KeywordNew

  case KeywordClass

  case KeywordExtends

  case Identifier(name: String)

  case LiteralInt(value: Int)

  case LiteralFloat(value: Double)

  case LiteralBoolean(value: Boolean)

  case LiteralString(value: String)

  case Semicolon

  case KeywordIf

  case KeywordThen

  case KeywordElse

  case KeywordWhile

  case KeywordDo

  case Plus

  case Minus

  case Asterisk

  case Slash

  case Caret

  case DoubleAmpersand

  case DoubleVerticalBar

  case GreaterThan

  case LessThan

  case GreaterThanEqual

  case LessThanEqual

  case BangEqual

  case EqualEqual

  case Bang

  case NewLine

  case EndOfSource

  case Error(msg: String)

}

case class ScalaToken(sourcePos: SourcePos, length: Int, tokenType: ScalaTokenType) {
  def showInSource: String = {
    val (lineNum, linePos, lineStr) = sourcePos.extractLine
    val header = s" $lineNum | "
    val signSpace = " ".repeat(header.length + linePos)
    val sign = "^".repeat(if length <= 0 then 1 else length)
    s"$header$lineStr\n$signSpace$sign"
  }

  override def toString: String = s"\n$showInSource\n"
}

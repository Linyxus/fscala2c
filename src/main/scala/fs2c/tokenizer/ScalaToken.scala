package fs2c.tokenizer

import fs2c.io.{Positional, SourcePos}

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

  case Percent

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

  def show: String = this match {
    case LeftBrace => "{"
    case RightBrace => "}"
    case LeftBracket => "["
    case RightBracket => "]"
    case LeftParen => "("
    case RightParen => ")"
    case KeywordVal => "val"
    case KeywordVar => "var"
    case KeywordDef => "def"
    case Equal => "="
    case Colon => ":"
    case Comma => ","
    case Dot => "."
    case BigRightArrow => "=>"
    case KeywordNew => "new"
    case KeywordClass => "class"
    case KeywordExtends => "extends"
    case Identifier(name: String) => s"@$name"
    case LiteralInt(value: Int) => value.toString
    case LiteralFloat(value: Double) => value.toString
    case LiteralBoolean(value: Boolean) => if value then "True" else "False"
    case LiteralString(value: String) => value
    case Semicolon => ";"
    case KeywordIf => "if"
    case KeywordThen => "then"
    case KeywordElse => "else"
    case KeywordWhile => "while"
    case KeywordDo => "do"
    case Plus => "+"
    case Minus => "-"
    case Asterisk => "*"
    case Slash => "/"
    case Caret => "^"
    case Percent => "%"
    case DoubleAmpersand => "&&"
    case DoubleVerticalBar => "||"
    case GreaterThan => ">"
    case LessThan => "<"
    case GreaterThanEqual => ">="
    case LessThanEqual => "<="
    case BangEqual => "!="
    case EqualEqual => "=="
    case Bang => "!"
    case NewLine => "<newline>"
    case EndOfSource => "<eof>"
    case Error(msg: String) => s"error($msg)"
  }
}

case class ScalaToken(tokenType: ScalaTokenType) extends Positional {
  override type PosSelf = ScalaToken

  def showToken: String = tokenType.show

  def showInSource: String = s"${pos.showInSourceLine} $showToken"
}

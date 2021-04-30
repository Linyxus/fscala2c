package fs2c.tokenizer

import fs2c.io.{ScalaSource, SourcePos, SourcePosSpan}
import fs2c.tools.fp.syntax._
import OptionSyntax._
import ScalaTokenType._

/** Tokenizer for Featherweight Scala.
  * Supports logical NewLine tokens based on indention level.
  *
  * @param source The input source file.
  */
class Tokenizer(val source: ScalaSource) {

  /** Tokenizer error with message `msg`.
    */
  case class TokenizerError(msg: String) extends Exception

  var start: Int = 0
  var current: Int = 0

  /** Indention-based statement syntax in block expressions.
    */

  /** Records whether new line character has been produced.
    */
  private var newLineProduced: Boolean = true
  /** Indention level stack.
    */
  private var indentLevels: List[Int] = List(0)
  /** Records whether the block opens a new indention level.
    */
  private var newLevelProduced: List[Boolean] = Nil
  /** Is about to start a new indention level.
    */
  private var blockStarting: Boolean = false

  def content: String = source.content

  def blockIndentLevel: Int = indentLevels.head

  /** Returns the current indention level, i.e. number of characters from line start.
    */
  def currentIndentLevel: Int = {
    var i = current - 1
    var x = 0
    while i >= 0 && content(i) != '\n' do {
      i -= 1
      x += 1
    }
    x
  }

  /** If there exists new lines between the current token and the left brace,
    * then a new indention level should be opened. Otherwise, do nothing.
    *
    * In both conditions, [[newLevelProduced]] will be updated to record
    * whether the current pair of brace has opened a new indention level.
    */
  def maybeStartLevel(): Unit = {
    newLevelProduced = crossLineEnd :: newLevelProduced
    if crossLineEnd then
      startBlock()
  }

  def maybeEndLevel(): Unit = {
    newLevelProduced match {
      case newLevel :: prev =>
        if newLevel then endBlock()
        newLevelProduced = prev
      case Nil =>
        throw TokenizerError("Unexpected `}` character")
    }
  }

  def startBlock(): Unit = {
    indentLevels = currentIndentLevel :: indentLevels
    blockStarting = false
    newLineProduced = true  // silencing the first newline token
  }

  def prepareStartBlock(): Unit =
    blockStarting = true

  def endBlock(): Unit = indentLevels = indentLevels match {
    case Nil =>
      throw TokenizerError("indent stack is empty; this must be a bug in the implementation; please open an issue")
    case _ :: Nil => throw TokenizerError("can not pop the root indent level")
    case _ :: xs => xs
  }

  private def currentPosSpan: SourcePosSpan =
    SourcePosSpan(source.locatePos(start), current - start)

  def makeToken(tokenType: ScalaTokenType): ScalaToken =
    ScalaToken(tokenType).withPos(currentPosSpan)

  def maybeProduceNewLine: Option[ScalaToken] = {
    if !newLineProduced && crossLineEnd && currentIndentLevel <= blockIndentLevel then {
      newLineProduced = true
      Some(makeToken(NewLine))
    } else {
      newLineProduced = false
      None
    }
  }

  def eof: Boolean = current == content.length

  def peek: Char = {
    assert(!eof, s"can not peek at end of file")
    content(current)
  }

  /** Step forward and return the current character.
    */
  def forward: Char = {
    val ch = peek
    current += 1
    ch
  }

  /** Return the next character.
    */
  def peekAhead: Char =
    if current + 1 < content.length then
      content(current + 1)
    else
      '\u0000'

  /** Look at the next character. If it matches any in the `chars`, then step forward _twice_,
    * return false else.
    */
  def lookAhead(chars: Char*): Boolean =
    if chars contains peekAhead then {
      forward;
      forward
      true
    } else {
      false
    }

  /** Look at the current character. If it matches any in the `chars`, then step forward,
    * return false else.
    */
  def look(chars: Char*): Boolean =
    (chars contains peek) && {
      forward;
      true
    }

  /** Records whether a line end is encountered while skipping spaces between tokens.
    */
  private var crossLineEnd = false

  /** Skip one space character, or a section of comment.
    */
  def skipSpace: Boolean =
    !eof && (
      ({
        Set(' ', '\n', '\t', '\r') contains peek
      } && {
        if Set('\n', '\r') contains forward then crossLineEnd = true;
        true
      }) || {
        peek == '/' && lookAhead('*') && {
          @annotation.tailrec def go(state: Int): Unit =
            forward match {
              case '*' =>
                go(1)
              case '/' if state == 1 =>
                ()
              case _ =>
                go(0)
            }

          go(0)
          true
        }
      }
      )

  /** Skip spaces until eof or the next character is neither space nor comment.
    */
  def skipSpaces(): Unit = {
    crossLineEnd = false
    while skipSpace do ()
  }

  def literalNumeric: LiteralFloat | LiteralInt = {
    while !eof && peek.isDigit do
      forward
    if !eof && peek == '.' then {
      forward
      while !eof && peek.isDigit do
        forward
      val floatStr = content.substring(start, current)
      LiteralFloat(floatStr.toDouble)
    } else {
      val intStr = content.substring(start, current)
      LiteralInt(intStr.toInt)
    }
  }

  /** Parse a symbol starting with a letter and consists of letters and digits.
    */
  def symbol: String = {
    while !eof && peek.isLetterOrDigit do
      forward
    content.substring(start, current)
  }

  /** Try to parse a keyword token from the given string.
    */
  def keyword(str: String): Option[ScalaTokenType] = str match {
    case "if" => Some(KeywordIf)
    case "val" => Some(KeywordVal)
    case "var" => Some(KeywordVar)
    case "def" => Some(KeywordDef)
    case "then" => Some(KeywordThen)
    case "else" => Some(KeywordElse)
    case "new" => Some(KeywordNew)
    case "class" => Some(KeywordClass)
    case "extends" => Some(KeywordExtends)
    case "True" => Some(LiteralBoolean(true))
    case "False" => Some(LiteralBoolean(false))
    case "true" => Some(LiteralBoolean(true))
    case "false" => Some(LiteralBoolean(false))
    case _ => None
  }

  def identifier(str: String): ScalaTokenType = Identifier(str)

  /** Skip spaces and get next token.
    * Will produce special token NewLine for a logical (indented) newline.
    * For example:
    * ``` scala
    * {
    *     val x =
    *         1
    *      // | Will not produce new line here
    *     val y = 1
    *  // | but will produce new line here (before KeywordVal)
    * }
    * ```
    */
  def nextToken: ScalaToken = {
    skipSpaces()
    start = current
    if blockStarting then maybeStartLevel()
    if eof then
      makeToken(EndOfSource)
    else maybeProduceNewLine match {
      case Some(tk) => tk
      case None => makeToken {
        forward match {
          case '{' =>
            // prepare to start a new indention level
            prepareStartBlock()
            LeftBrace
          case '}' =>
            // end the current indention level if exists
            maybeEndLevel()
            RightBrace
          case '[' => LeftBracket
          case ']' => RightBracket
          case '(' => LeftParen
          case ')' => RightParen
          case '=' if look('>') => BigRightArrow
          case '=' if look('=') => EqualEqual
          case '=' => Equal
          case ':' => Colon
          case ';' => Semicolon
          case ',' => Comma
          case '.' => Dot
          case '+' => Plus
          case '-' => Minus
          case '*' => Asterisk
          case '/' => Slash
          case '^' => Caret
          case '&' if look('&') => DoubleAmpersand
          case '|' if look('|') => DoubleVerticalBar
          case '>' if look('=') => GreaterThanEqual
          case '>' => GreaterThan
          case '<' if look('=') => LessThanEqual
          case '<' => LessThan
          case '!' if look('=') => BangEqual
          case '!' => Bang
          case ch if ch.isLetter =>
            val str = symbol
            keyword(str) !! identifier(str)
          case ch if ch.isDigit =>
            literalNumeric
          case ch => Error("unexpected character")
        }
      }
    }
  }

  def allTokens: List[ScalaToken] = nextToken match {
    case t @ ScalaToken(ScalaTokenType.EndOfSource) => List(t)
    case t => t :: allTokens
  }

  def allTokensLazy: LazyList[ScalaToken] = nextToken match {
    case t @ ScalaToken(ScalaTokenType.EndOfSource) => LazyList(t)
    case t => t #:: allTokensLazy
  }
}

object Tokenizer {
  def tokenize(source: ScalaSource): List[ScalaToken] =
    new Tokenizer(source).allTokens
}

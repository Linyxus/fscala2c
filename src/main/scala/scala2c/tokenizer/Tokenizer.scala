package scala2c.tokenizer

import scala2c.io.{Source, SourcePos}
import scala2c.tools.fp.syntax._
import ScalaTokenType._

class Tokenizer(val source: Source) extends OptionSyntax {
  
  case class TokenizerError(msg: String) extends Exception
  
  var start: Int = 0
  var current: Int = 0
  var newLineProduced: Boolean = true
  var indentLevels: List[Int] = List(0)
  var blockStarting: Boolean = false

  def content: String = source.content

  def blockIdentLevel: Int = indentLevels.head

  def currentIdentLevel: Int = {
    var i = current - 1
    var x = 0
    while i >= 0 && content(i) != '\n' do {
      i -= 1
      x += 1
    }
    x
  }

  def startBlock(): Unit =
    indentLevels = start :: indentLevels
  
  def prepareStartBlock(): Unit =
    blockStarting = true

  def endBlock(): Unit = indentLevels = indentLevels match {
    case Nil => 
      throw TokenizerError("indent stack is empty; this must be a bug in the implementation; please open an issue")
    case _ :: Nil => throw TokenizerError("can not pop the root indent level")
    case _ :: xs => xs
  }

  def makeToken(tokenType: ScalaTokenType): ScalaToken =
    ScalaToken(SourcePos(source, start), current - start, tokenType)

  def maybeProduceNewLine: Option[ScalaToken] =
    if !newLineProduced && crossLineEnd && currentIdentLevel <= blockIdentLevel then {
      newLineProduced = true
      Some(makeToken(NewLine))
    } else {
      newLineProduced = false
      None
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
   *   return false else.
   */
  def lookAhead(chars: Char*): Boolean =
    if chars contains peekAhead then {
      forward; forward
      true
    } else {
      false
    }

  /** Look at the current character. If it matches any in the `chars`, then step forward,
   *   return false else.
   */
  def look(chars: Char*): Boolean =
    (chars contains peek) && { forward; true }

  var crossLineEnd = false
  /** Skip one space character, or a section of comment.
   */
  def skipSpace: Boolean =
    !eof && (
      ({ Set(' ', '\n', '\t', '\r') contains peek } &&
        { if Set('\n', '\r') contains forward then crossLineEnd = true; true }) || {
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
    case _ => None
  }

  def identifier(str: String): ScalaTokenType = Identifier(str)

  /** Skip spaces and get next token.
   *  Will produce special token NewLine for a logical (indented) newline.
   *  For example:
   *  {
   *    val x =
   *      1
   *      ^ Will not produce new line here
   *    val y = 1
   *    ^ but will produce new line here (before KeywordVal)
   *  }
   */
  def nextToken: ScalaToken = {
    skipSpaces()
    start = current
    if blockStarting then startBlock()
    if eof then
      makeToken(EndOfSource)
    else maybeProduceNewLine match {
      case Some(tk) => tk
      case None => makeToken {
        forward match {
          case '{' => LeftBrace
          case '}' => RightBrace
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
          case '|' if look('|') => DoubleVertialBar
          case '>' if look('=') => GreaterThanEqual
          case '>' => GreaterThan
          case '<' if look('=') => LessThanEqual
          case '<' => LessThan
          case '!' if look('=') => BangEqual
          case '!' => Bang
          case ch if ch.isLetter =>
            val str = symbol
            keyword(str) !! identifier(str)
          case ch => Error("unexpected character")
        }
      }
    }
  }

  def allTokens: List[ScalaToken] = nextToken match {
    case t @ ScalaToken(_, _, ScalaTokenType.EndOfSource) => List(t)
    case t => t :: allTokens
  }
  
  def allTokensLazy: LazyList[ScalaToken] = nextToken match {
    case t @ ScalaToken(_, _, ScalaTokenType.EndOfSource) => LazyList(t)
    case t => t #:: allTokensLazy
  }
}

object Tokenizer {
  def tokenize(source: Source): List[ScalaToken] =
    new Tokenizer(source).allTokens
}

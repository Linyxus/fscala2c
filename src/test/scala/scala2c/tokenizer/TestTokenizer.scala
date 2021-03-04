package scala2c.tokenizer

import org.junit.Test
import org.junit.Assert._
import scala2c.io._
import scala2c.tokenizer._
import ScalaTokenType._

class TestTokenizer {
  @Test def operators: Unit = {
    val source = Source("test", "+-*/^")
    val tokens = Tokenizer.tokenize(source)
    assertEquals(List(Plus, Minus, Asterisk, Slash, Caret, EndOfSource), tokens map (_.tokenType))
  }
  
  @Test def newLine: Unit = {
    val source = Source("test", "hello\n  world\nhello\nworld")
    val tokens = Tokenizer.tokenize(source)
    assertEquals(
      List(Identifier("hello"), Identifier("world"), NewLine, Identifier("hello"), NewLine, Identifier("world"), EndOfSource), 
      tokens map (_.tokenType)
    )
  }
}

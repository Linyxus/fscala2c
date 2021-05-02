package fs2c.tokenizer

import org.junit.Test
import org.junit.Assert._
import fs2c.io._
import fs2c.tokenizer._
import ScalaTokenType._

class TestTokenizer {
  @Test def operators: Unit = {
    val source = ScalaSource.testSource("+-*/^")
    val tokens = Tokenizer.tokenize(source)
    assertEquals(List(Plus, Minus, Asterisk, Slash, Caret, EndOfSource), tokens map (_.tokenType))
  }

  @Test def newLine: Unit = {
    val source = ScalaSource.testSource("hello\n  world\nhello\nworld")
    val tokens = Tokenizer.tokenize(source)
    assertEquals(
      List(Identifier("hello"), Identifier("world"), NewLine, Identifier("hello"), NewLine, Identifier("world"), EndOfSource), 
      tokens map (_.tokenType)
    )
  }

  @Test def logicalNewLine: Unit = {
    val tests = List(
      """val a = b
        |  + c""".stripMargin -> "val @a = @b + @c <eof>",
      """val a = { b }""" -> "val @a = { @b } <eof>",
      """val a = {
        |  val t = 2
        |    * a
        |  t + 1
        |}""".stripMargin -> "val @a = { val @t = 2 * @a <newline> @t + 1 <newline> } <eof>",
    )

    tests foreach { (i, e) =>
      val source = ScalaSource.testSource(i)
      val tokens = Tokenizer.tokenize(source)
      val o = tokens map (_.showToken) mkString " "
      assertEquals(e, o)
    }
  }
}

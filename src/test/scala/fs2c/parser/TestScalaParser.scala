package fs2c.parser

import scala.language.implicitConversions

import fs2c.io.ScalaSource
import fs2c.tokenizer.{ScalaToken, ScalaTokenType, Tokenizer}
import ScalaTokenType._
import fs2c.tools.packratc.scala_token.ScalaTokenParser.{given, *}
import fs2c.tools.packratc.scala_token._
import fs2c.parser.ScalaParser
import org.junit.Assert._
import org.junit.Test
import fs2c.tools.packratc.Parser.~

class TestScalaParser {
  def assertParseSuccess[X](parser: Parser[X], string: String): Unit = {
    val p = parser << EOF
    val res = ScalaParser.parseString(p, string)
    assertTrue(res.isRight)
  }

  def assertParseFailure[X](parser: Parser[X], string: String): Unit = {
    val p = parser << EOF
    assertTrue(ScalaParser.parseString(p, string).isLeft)
  }

  @Test def simpleExpr: Unit = {
    val scalaParser = new ScalaParser

    val successStr = List(
      "a",
      "a + b",
      "-a",
      "-a + b",
      "a + b - c - d",
      "a + b * c",
      "a + b * (c + -d)",
      "a + b * (c + -d) ^ e",
      "1 > 2",
      "1 >= 2",
      "1 <= 2",
      "1 < 2",
      "(1 + 1) == 2",
    )

    val failureStr = List(
      "a -",
      "a + (b - c",
      "(((a))"
    )

    successStr foreach { s => assertParseSuccess(scalaParser.exprParser, s) }
    failureStr foreach { s => assertParseFailure(scalaParser.exprParser, s) }
  }

  @Test def literals: Unit = {
    val tests = List(
      "1", "1.0", "True", "False",
      "1 + 1.0", "2.0 + 3.0", "1.1231 + False * True"
    )
    tests foreach { s => assertParseSuccess((new ScalaParser).exprParser, s) }
  }

  @Test def typeParser: Unit = {
    val tests = List(
      "Int", "Float", "Boolean", "String", "Unit",
      "Int => Int", "(Int, Int) => Int", "(Int, Int) => Int => Int",
      "Int => (Int => Int) => Int", "Int => (Array[Int] => Int) => Array[Int => String]",
      "Foo => Bar", "Int => Unit"
    )

    tests foreach { s => assertParseSuccess((new ScalaParser).typeParser, s) }
  }

  @Test def blockExpr: Unit = {
    assertParseSuccess((new ScalaParser).blockExpr,
      """{
        |  val x = {
        |    val b = a * a
        |    a + b
        |  }
        |  x
        |}""".stripMargin)

    assertParseSuccess((new ScalaParser).blockExpr,
      """{
        |  val x : Int => Int => Int = (y : Int) => {
        |    (x : Int) => {
        |      x + y
        |    }
        |  }
        |  var i : Int = a
        |  i = i + 1
        |  x + i
        |}""".stripMargin)
  }

  @Test def whileStatement: Unit = {
    assertParseSuccess((new ScalaParser).blockExpr,
      """{
        |  var x = 0
        |  while x < 100 do {
        |    x = x + 1
        |    x + 1
        |  }
        |  x
        |}
        |""".stripMargin)
  }

  @Test def lambda: Unit = {
    assertParseSuccess((new ScalaParser).lambdaExpr,
      """(x : Int) => {
        |  x + a
        |}""".stripMargin)
    assertParseSuccess((new ScalaParser).lambdaExpr,
      """(x : Int) => (a : Int) => {
        |  x + a
        |}""".stripMargin)
    assertParseSuccess((new ScalaParser).lambdaExpr, "(x : Int) => x * x")
    assertParseSuccess((new ScalaParser).lambdaExpr, "(x : Int, y : Int) => x + y")
    assertParseSuccess((new ScalaParser).lambdaExpr, "(x : Int, y : Int) => (z : Int) => x + y * z")
  }
  
  @Test def memberDef: Unit = {
    assertParseSuccess((new ScalaParser).memberDef, "val x : Int = a")
    assertParseSuccess((new ScalaParser).memberDef, "val x = a")
    assertParseSuccess((new ScalaParser).memberDef, "var aLongName123 = a")
    assertParseSuccess((new ScalaParser).memberDef, "val func : (Int, Int) => Int = a")
    assertParseSuccess((new ScalaParser).memberDef, "val func : (Int, Int) => Int = (x : Int, y : Int) => x + y")
  }

  @Test def classDef: Unit = {
    val tests = List(
      """class Foo {
        |}""".stripMargin,
      """class Foo(x1 : Int, y1 : Int) extends Bar {
        |  val x = x1
        |  val y = y1
        |
        |  val addX = (a : Int) => a + x
        |
        |  val func = (a : Int, b : Int) => {
        |    val t1 = a * x
        |    val t2 = b ^ y
        |    t1 + t2
        |  }
        |}""".stripMargin,
      """class Foo {
        |  def m1: Int = 1
        |  def m2(a: Int, b: Float): Int = a
        |  def m3 = 1
        |  def m4(x: String) = x
        |}""".stripMargin
    )
    tests foreach { x => assertParseSuccess((new ScalaParser).classDefParser, x) }
  }
  
  @Test def applyOrSelect: Unit = {
    val tests = List(
      "foo.bar",
      "foo.bar.baz",
      "foo.bar.baz(a, b).c"
    )
    
    tests foreach { x => assertParseSuccess((new ScalaParser).exprParser, x) }
  }
  
  @Test def ifExpr: Unit = {
    val tests = List(
      """if true then {
        |  val x = 1
        |  var y = 10
        |  y = y + x
        |  x * y
        |} else 1 + 1""".stripMargin,
      "if true then false else true",
      "if n == 0 then 1 else n * fact(n - 1)",
      """if n >= 0 then
        |  1
        |else
        |  n * fact(n - 1)""".stripMargin
    )
    
    tests foreach { x => assertParseSuccess((new ScalaParser).ifExpr, x) }
  }
  
  @Test def newExpr: Unit = {
    val tests = List(
      "new Foo",
      "new Foo()",
      "(new Foo(a, b, c)).bar",
      "(new Foo(a, b, c)).bar(1, 2, 3)"
    )
    
    tests foreach { x => assertParseSuccess((new ScalaParser).exprParser, x) }
  }

  @Test def localDefDef: Unit = {
    val tests = List(
      """{
        |  val x = 1
        |  def addX(n: Int) = x + n
        |
        |  addX(x * 2)
        |}
        |""".stripMargin,
      """{
        |  def odd(n: Int) =
        |    if n == 0 then
        |      false
        |    else
        |      !even(n - 1)
        |
        |  def even(n: Int) =
        |    if n == 0 then
        |      true
        |    else
        |      !odd(n - 1)
        |
        |  even(2) && !odd(2)
        |}
        |""".stripMargin,
    )

    tests foreach { x => assertParseSuccess((new ScalaParser).exprParser, x) }
  }

  @Test def main: Unit = {
    val tests = List(
      """class Foo {
        |}
        |
        |class Bar {
        |}
        |""".stripMargin,
      """class Foo {
        |}
        |
        |class Bar {
        |  val x = new Foo
        |}
        |""".stripMargin,
    )

    tests foreach { x => assertParseSuccess((new ScalaParser).mainParser, x) }
  }
}

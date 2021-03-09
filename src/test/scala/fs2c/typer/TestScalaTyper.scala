package fs2c.typer

import fs2c.io.ScalaSource
import fs2c.parser.ScalaParser
import fs2c.tokenizer.{ScalaToken, ScalaTokenType, Tokenizer}
import fs2c.tools.packratc.Parser.~
import fs2c.tools.packratc.scala_token.ScalaTokenParser.*
import fs2c.tools.packratc.scala_token._
import fs2c.ast.fs._
import Trees._
import fs2c.typer._
import Types._
import GroundType._

import org.junit.Assert._
import org.junit.Test

import scala.language.implicitConversions

class TestScalaTyper {
  def forceParseString(source: String): untpd.Expr = {
    ScalaParser.parseString((new ScalaParser).exprParser, source) match {
      case Left(value) => ???
      case Right(value) => value._1
    }
  }
  
  def typedExpr(e: untpd.Expr): tpd.Expr = (new Typer).typedExpr(e)
  
  def assertTyped(str: String, tpe: Type): Unit =
    assertEquals(typedExpr(forceParseString(str)).tpe, tpe)
  
  @Test def simpleExpr: Unit = {
    assertTyped("1", GroundType.IntType)
    assertTyped("1 + 1", GroundType.IntType)
    assertTyped("1 + 1 * (2 + 100)", GroundType.IntType)
    assertTyped("1.0 + 1.0 * (2.0 + 100.0)", GroundType.FloatType)
    assertTyped("1.0 + 1.0 * (2.0 + 100.0) == 10.0", GroundType.BooleanType)
    assertTyped("2 > 3 && 3 < 4", GroundType.BooleanType)
    assertTyped("2 > 3 && 3 != 4", GroundType.BooleanType)
    assertTyped("2 == 3 && 3 != 4", GroundType.BooleanType)
  }
}

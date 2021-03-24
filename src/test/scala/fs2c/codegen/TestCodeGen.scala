package fs2c.codegen

import fs2c.io.ScalaSource
import fs2c.parser.ScalaParser
import fs2c.tokenizer.{ScalaToken, ScalaTokenType, Tokenizer}
import fs2c.tools.packratc.Parser.~
import fs2c.tools.packratc.scala_token.ScalaTokenParser.*
import fs2c.tools.packratc.scala_token._
import fs2c.ast.fs.{Trees => FS}
import fs2c.typer.{ Typer, Types => FST }
import fs2c.codegen.{ CodeBundles => bd }

import org.junit.Assert._
import org.junit.Test

import scala.language.implicitConversions

class TestCodeGen {
  def forceParseString(source: String): FS.untpd.Expr = {
    ScalaParser.parseString((new ScalaParser).exprParser, source) match {
      case Left(value) =>
        assert(false, value.toString)
      case Right(value) => value._1
    }
  }

  def typedExpr(e: FS.untpd.Expr): FS.tpd.Expr = (new Typer).typedExpr(e)
  
  def typedString(source: String): FS.tpd.Expr = typedExpr(forceParseString(source))
  
  def genExpr(expr: FS.tpd.Expr): bd.ValueBundle = (new CodeGen).genExpr(expr)
  
  @Test def simpleExpr: Unit = {
    val tests = List(
      (
        "1",
        "1"
      ),
      (
        "1 + 1 * 2",
        "(+ 1 (* 1 2))"
      ),
      (
        "1.1 + 2.3 * 4.4",
        "(+ 1.1 (* 2.3 4.4))"
      ),
      (
        "1 > 2 && false",
        "(&& (> 1 2) false)"
      ),
      (
        "1 == 2 && false",
        "(&& (== 1 2) false)"
      ),
    )
    tests foreach { (i, o) => assertEquals(genExpr(typedString(i)).getExpr.toString, o) }
  }
}

package fs2c.codegen

import scala.language.implicitConversions
import fs2c.printing.Printer.{*, given}
import fs2c.printing.printing.c
import c.{*, given}

import fs2c.io.ScalaSource
import fs2c.parser.ScalaParser
import fs2c.tokenizer.{ScalaToken, ScalaTokenType, Tokenizer}
import fs2c.tools.packratc.Parser.~
import fs2c.tools.packratc.scala_token.ScalaTokenParser.*
import fs2c.tools.packratc.scala_token._
import fs2c.ast.fs.{Trees => FS}
import fs2c.ast.c.{Trees => C}
import fs2c.typer.{ Typer, Types => FST }
import fs2c.codegen.{ CodeBundles => bd, CodePrinter }

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

  def forceParseStringDef(source: String): FS.untpd.ClassDef =
    ScalaParser.parseString((new ScalaParser).classDefParser, source) match {
      case Left(value) =>
        assert(false, value.toString)
      case Right(value) => value._1
    }

  def typedExpr(e: FS.untpd.Expr): FS.tpd.Expr = (new Typer).typedExpr(e)

  def typedClassDef(clsDef: FS.untpd.ClassDef): FS.tpd.ClassDef = (new Typer).typedClassDef(clsDef)

  def typedString(source: String): FS.tpd.Expr = typedExpr(forceParseString(source))

  def typedStringDef(source: String): FS.tpd.ClassDef = typedClassDef(forceParseStringDef(source))

  def genExpr(expr: FS.tpd.Expr): bd.ValueBundle = {
    val codegen = new CodeGen
    val res = codegen.genExpr(expr)
    
    res
  }
  
  def genExprAndDef(expr: FS.tpd.Expr): (bd.ValueBundle, List[C.Definition]) = {
    val codegen = new CodeGen
    val res = codegen.genExpr(expr)

    res -> codegen.ctx.generatedDefs
  }
    

  def genType(tpe: FST.Type): bd.TypeBundle = (new CodeGen).genType(tpe)

  def genClassDef(clsDef: FS.tpd.ClassDef): (bd.ClassBundle, CodeGen) = {
    val codegen = new CodeGen

    codegen.genClassDef(clsDef) -> codegen
  }

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
  
  @Test def ifExpr: Unit = {
    val e = typedString("(if false then 1 else 0) * (if false then 3 else 2)")
    genExpr(e)
  }
  
  @Test def simpleTypes: Unit = {
    import FST.*
    import GroundType.*
    
    val tests = List(
      IntType -> bd.SimpleTypeBundle(C.BaseType.IntType),
      FloatType -> bd.SimpleTypeBundle(C.BaseType.DoubleType),
      BooleanType -> bd.SimpleTypeBundle(C.BaseType.IntType),
    )
    
    tests foreach { (i, o) => assertEquals(o, genType(i)) }
  }
  
  @Test def simpleLambda: Unit = {
    val tests = List(
      "(x : Int) => 10",
      "(x : Int, y : Int) => x + y + 1",
      "(x : Int, y : Int) => 10.0",
    )
    
    tests foreach { i => genExpr(typedString(i)) }
  }
  
  @Test def closureLambda: Unit = {
    val e = typedString(
      """{
        |  val makeAdder = (n : Int) => {
        |    val adder = (x : Int) => x + n
        |    adder
        |  }
        |  val addOne = makeAdder(1)
        |  addOne
        |}
        |""".stripMargin)
    val (expr, defs) = genExprAndDef(e)
  }
  
  @Test def blockExpr: Unit = {
    val source =
      """{
        |  val x = 1
        |  val y = 2
        |  val foo = (x : Int, y : Int) => x + 2 * y
        |  x + y
        |}
        |""".stripMargin
    val e = typedString(source)
    genExpr(e)
  }

  import C.BaseType.*
  import C.*

  @Test def printSimpleTypes: Unit = {
    val tests = Seq(
      IntType -> "int",
      DoubleType -> "double",
      CharType -> "char",
      FuncType(FuncType(VoidType, List(IntType)), List(DoubleType, CharType)) -> "void (*) (int) (*) (double, char)",
    )
    
    tests foreach { (i, o) => assertEquals(o, i.show) 
    }
  }
  
  @Test def printSimpleExpr: Unit = {
    val tests = List(
      "1 + 2 * 3" -> "1 + 2 * 3",
      "(1 + 2) * 3" -> "(1 + 2) * 3",
      "(1 + 2 ^ 3) * 3" -> "(1 + 2 ^ 3) * 3",
      "1 > 2 && False" -> "1 > 2 && 0",
    )
    
    tests foreach { (i, o) => assertEquals(o, genExpr(typedString(i)).getExpr.show) }
  }
  
  @Test def printFuncDef: Unit = {
    val e = typedString(
      """() => (x : Int, y : Int) => {
        |  val mult = (a : Int, b : Int) => 2 * a * b
        |  mult
        |}
        |""".stripMargin)
    val (_, defs) = genExprAndDef(e)
    defs.reverse foreach { d => d.show }
  }
  
  @Test def printLambdaRelated: Unit = {
    val e = typedString(
      """
        |{
        |  val main = () => {
        |    val apply = (func : Int => Int, i : Int) => func(i)
        |    val sq = (i : Int) => i * i
        |    apply(sq, 10)
        |  }
        |  0
        |}
        |""".stripMargin)
    val (_, defs) = genExprAndDef(e)
    defs.reverse foreach { d => d.show }
  }
  
  @Test def printLambdaRec: Unit = {
    val e = typedString(
      """
        |{
        |  val main = () => {
        |    def odd(n : Int) =
        |      if n == 0 then
        |        false
        |      else
        |        even(n - 1)
        |    def even(n : Int) =
        |      if n == 0 then
        |        true
        |      else
        |        odd(n - 1)
        |    even(0)
        |  }
        |  0
        |}
        |""".stripMargin)
    val (_, defs) = genExprAndDef(e)
    defs.reverse foreach { d => println(d.show) }
  }

  @Test def printLambdaRec2: Unit = {
    val e = typedString(
      """
        |{
        |  val main = () => {
        |    def sum(n : Int) =
        |      if n == 0 then
        |        0
        |      else
        |        n + sum(n - 1)
        |
        |    sum(10)
        |  }
        |  0
        |}
        |""".stripMargin)
    val (_, defs) = genExprAndDef(e)
  }

  @Test def printSimpleClass: Unit = {
    val d = typedStringDef(
      """class Point(x0: Int, y0: Int) {
        |  val x = x0
        |  val y = y0
        |  val origin = new Point(0, 0)
        |  val mid = (other: Point) => {
        |    val mx = (x + other.x) / 2
        |    val my = (y + other.y) / 2
        |    new Point(mx, my)
        |  }
        |  val midOrigin = mid(origin)
        |}""".stripMargin
    )

    val (_, gen) = genClassDef(d)

    val printer = new CodePrinter(gen.ctx.generatedDefs.reverse, gen.ctx.included)
    println(printer.sourceContent)
  }
}

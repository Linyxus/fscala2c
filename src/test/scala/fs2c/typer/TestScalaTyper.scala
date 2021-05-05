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
      case Left(value) =>
        assert(false, value.toString)
      case Right(value) => value._1
    }
  }

  def forceParseDefString(source: String): untpd.ClassDef = {
    ScalaParser.parseString((new ScalaParser).classDefParser, source) match {
      case Left(_) => ???
      case Right(x) => x._1
    }
  }

  def typedExpr(e: untpd.Expr): tpd.Expr =
    val typer = new Typer
    val res = typer.typedExpr(e)
    typer.forceInstantiateBlock()
    res

  def typedDef(d: untpd.ClassDef): tpd.ClassDef = (new Typer).typedClassDef(d)

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
    assertTyped("-1", IntType)
    assertTyped("-(1 + 1 * -1)", IntType)
    assertTyped("!(1 > 2 && 3 < 4)", BooleanType)
    assertTyped("Array[Int](10)", ArrayType(IntType))
    assertTyped("Array[Int](1 + 1)", ArrayType(IntType))
  }

  @Test def groundValue: Unit = {
    assertTyped("printInt", LambdaType(List(IntType), IntType))
    assertTyped("printInt(1 + 1)", IntType)
    assertTyped("readInt", LambdaType(Nil, IntType))
    assertTyped("readInt()", IntType)
    assertTyped("readInt() + 1", IntType)
  }

  @Test def strings: Unit = {
    assertTyped("\"one\"", StringType)
    assertTyped(
      """{
        |  val x = "one"
        |  x
        |}""".stripMargin, StringType)
  }

  @Test def lambdaExpr: Unit = {
    assertTyped("(x : Int) => x", LambdaType(List(IntType), IntType))
    assertTyped("(x : Int, y : Int) => x + y + 2 * x * y", LambdaType(List(IntType, IntType), IntType))
    assertTyped("(x : Int, y : Int) => x + y > x * y", LambdaType(List(IntType, IntType), BooleanType))
    assertTyped("(x : Int) => (y : Int) => x + y", LambdaType(List(IntType), LambdaType(List(IntType), IntType)))
  }

  @Test def blockExpr: Unit = {
    val tests = List(
      (
        """{
          |  val x = 1
          |  val y = x + 1
          |  x + y
          |}""".stripMargin,
        IntType
      ),
      (
        """{
          |  var x: Int = 1
          |  val y = x + 1
          |  x = x + y
          |  x * y
          |}""".stripMargin,
        IntType
      ),
      (
        """{
          |  var x = 1
          |  val add = (y : Int) => {
          |    x = x + y
          |    x
          |  }
          |  add
          |}""".stripMargin,
        LambdaType(List(IntType), IntType)
      ),
      (
        """{
          |  val odd = (n : Int) => !(n == 0) && !even(n - 1)
          |  val even = (n : Int) => n == 0 || !odd(n - 1)
          |  odd
          |}""".stripMargin,
        LambdaType(List(IntType), BooleanType)
      ),
      (
        """{
          |  val fact = (n : Int) => n * fact(n - 1)
          |  fact
          |}""".stripMargin,
        LambdaType(List(IntType), IntType)
      ),
    )

    tests foreach { case (s, t) => assertTyped(s, t) }
  }

  @Test def whileStatement: Unit = {
    val tests = List(
      """{
        |  var x = 0
        |  var y = 1.0
        |  while x < 100 do {
        |    y = y * 2.0
        |    y
        |  }
        |
        |  y * y
        |}""".stripMargin -> FloatType
    )

    tests foreach { case (i, e) => assertTyped(i, e) }
  }

  @Test def applyExpr: Unit = {
    val tests = List(
      (
        """{
          |  val func = (x : Int) => (y : String) => y
          |  func(1)
          |}""".stripMargin,
        LambdaType(List(StringType), StringType)
      ),
      (
        """{
          |  val add = (x : Int, y : Int) => x + y
          |  val curry2 = (func: (Int, Int) => Int) => (x : Int) => (y : Int) => func(x, y)
          |  val add10 = curry2(add)(10)
          |  var x = 10
          |  x = add10(x)
          |  x
          |}""".stripMargin,
        IntType
      ),
      (
        """{
          |  var x = 0
          |  val adder = (y : Int) => {
          |    x = x + y
          |    x
          |  }
          |  adder(x)
          |  adder(x)
          |  x > 10
          |}""".stripMargin,
        BooleanType
      ),
    )

    tests foreach { case (s, t) => assertTyped(s, t) }
  }

  @Test def ifExpr: Unit = {
    val tests = List(
      (
        """{
          |  val fact = (n : Int) => {
          |    if n == 0 then
          |      1
          |    else
          |      n * fact(n - 1)
          |  }
          |
          |  fact
          |}""".stripMargin,
        LambdaType(List(IntType), IntType)
      ),
      (
        """{
          |  val odd = (n : Int) =>
          |    if n == 0 then False else !even(n - 1)
          |  val even = (n : Int) =>
          |    if n == 0 then True else !odd(n - 1)
          |  odd
          |}""".stripMargin,
        LambdaType(List(IntType), BooleanType)
      ),
      (
        """{
          |  val x = 0
          |  val y = if x < 0 then 0 else x
          |  x + y
          |}""".stripMargin,
        IntType
      ),
    )

    tests foreach { case (s, t) => assertTyped(s, t) }
  }

  @Test def array: Unit = {
    assertTyped(
      """{
        |  val xs = Array[Int](10)
        |  xs(0)
        |}
        |""".stripMargin, IntType)
  }

  @Test def classDef: Unit = {
    val source =
      """class Counter(initial : Int) {
        |  var cnt = initial
        |  
        |  val inc = () => {
        |    val old = cnt
        |    cnt = cnt + 1
        |    old
        |  }
        |  
        |  val incOther = (other : Counter) => other.inc()
        |  
        |  val likeOther = (other : Counter) => {
        |    val old = cnt
        |    cnt = other.cnt
        |    old
        |  }
        |  
        |  val newAndInc = () => {
        |    val counter = new Counter(cnt)
        |    counter.inc()
        |    counter
        |  }
        |}
        |""".stripMargin

    val d = forceParseDefString(source)
    val typer = new Typer
    typer.typedClassDef(d)
    
    val tests = List(
      (
        """{
          |  val x = {
          |    val counter = new Counter(0)
          |    counter.inc()
          |  }
          |  x
          |}""".stripMargin,
        IntType
      ),
      (
        """{
          |  val counter = new Counter(10)
          |  counter.inc
          |}""".stripMargin,
        LambdaType(List(), IntType)
      ),
      (
        """{
          |  val counter = new Counter(10)
          |  counter.newAndInc().newAndInc().newAndInc().inc
          |}""".stripMargin,
        LambdaType(List(), IntType)
      ),
    )

    tests foreach { case (s, t) => assertEquals(typer.typedExpr(forceParseString(s)).tpe, t) }
  }
  
  @Test def freeNames: Unit = {
    val source =
      """class Foo {
        |  val i = 0
        |  val j = 0
        |  val k = 0
        |  
        |  val get0 = () => i
        |  
        |  val get1 = (i : Int) => i
        |  
        |  val get2 = () => i + j - (i * j)
        |  
        |  val get3 = (i : Int) => add(i, j)
        |  
        |  val add = (i : Int, j : Int) => i + j + k
        |}
        |""".stripMargin

    val d = forceParseDefString(source)
    val typer = new Typer
    val res = typer.typedClassDef(d)
    
    def locateMemberFreeNames(name: String): Set[String] = {
      def recur(ds: List[tpd.MemberDef]): List[String] = ds match {
        case Nil => 
          assert(ds.nonEmpty, f"reach end of members, but can not find $name")
          ???
        case d :: ds if d.tree.sym.name == name => d.freeNames.map { x => x.name }
        case _ :: ds => recur(ds)
      }
      Set.from(recur(res.tree.members))
    }
    
    val tests = List(
      "get0" -> Set("i"),
      "get1" -> Set.empty,
      "get2" -> Set("i", "j"),
      "get3" -> Set("add", "j"),
      "add" -> Set("k"),
    )
    
    tests foreach { case (i, o) => assertEquals(o, locateMemberFreeNames(i)) }
  }
}

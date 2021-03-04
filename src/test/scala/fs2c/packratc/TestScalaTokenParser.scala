package fs2c.packratc

import scala.language.implicitConversions

import fs2c.io.ScalaSource
import fs2c.tokenizer.{ScalaToken, ScalaTokenType, Tokenizer}
import ScalaTokenType._
import fs2c.tools.packratc.scala_token.ScalaTokenParser.{given, *}
import fs2c.tools.packratc.scala_token._
import org.junit.Assert._
import org.junit.Test

class TestScalaTokenParser {
  def assertParseFailure[X](result: Result[X]): Unit = assertFalse(result.isRight)
  
  def assertParseSuccess[X](result: Result[X], value: X): Unit = assertTrue {
    result match {
      case Right(res, _) if res == value => true
      case _ => false
    }
  }
  
  def tokenizerFromStr(sourceStr: String): Tokenizer = {
    new Tokenizer(ScalaSource("test", sourceStr))
  }
  
  def parseOnStr[X](parser: Parser[X], sourceStr: String): Result[X] = {
    val tokenizer = tokenizerFromStr(sourceStr)
    parseWithTokenizer(parser, tokenizer)
  }
  
  @annotation.tailrec private def foldTree[X](cons: (X, X) => X, x: X, xs: List[X]): X = xs match {
    case Nil => x
    case y :: ys => foldTree(cons, cons(x, y), ys)
  }
  
  enum Expr {
    case Symbol(name: String)
    case Plus(e1: Expr, e2: Expr)
    case Mult(e1: Expr, e2: Expr)

    override def toString: String = this match {
      case Symbol(name) => name
      case Plus(e1, e2) => s"(+ $e1 $e2)"
      case Mult(e1, e2) => s"(* $e1 $e2)"
    }
  }

  lazy val symb: Parser[Expr] = identifier <| { case ScalaToken(_, _, Identifier(name)) => Expr.Symbol(name) } | {
    ("(" ~ expr ~ ")") <| { case ((_, e), _) => e }
  }
  lazy val factor: Parser[Expr] = { symb ~ ("*" ~ symb).many } <| { case (x, xs) =>
    val ys = xs map (_._2)
    foldTree((a, b) => Expr.Mult(a, b), x, ys)
  }
  lazy val expr: Parser[Expr] = { factor ~ ("+" ~ factor).many } <| { case (x, xs) =>
    val ys = xs map (_._2)
    foldTree((a, b) => Expr.Plus(a, b), x, ys)
  }
  
  @Test def simpleExpr: Unit = {
    def testSuccess(input: String, expect: String): Unit = {
      val p = (expr << "<EOF>") <| (_.toString)
      val res = parseOnStr(p, input)
      assertParseSuccess(res, expect)
    }
    
    def testFailure(input: String): Unit = {
      val p = (expr << "<EOF>") <| (_.toString)
      val res = parseOnStr(p, input)
      assertParseFailure(res)
    }
    
    testSuccess("a", "a")
    testSuccess("a + b", "(+ a b)")
    testSuccess("(a + b) * (c + d)", "(* (+ a b) (+ c d))")
    testSuccess("a * b * c", "(* (* a b) c)")
    testSuccess("a * b * c + c", "(+ (* (* a b) c) c)")
    
    testFailure("a+")
    testFailure("+")
    testFailure("a * (b + c")
    testFailure("a * (b ++ c)")
  }
}

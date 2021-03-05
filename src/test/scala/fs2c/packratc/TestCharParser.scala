package fs2c.packratc

import org.junit.Test
import org.junit.Assert._

import fs2c.tools.packratc.char._
import CharParser._

import fs2c.tools.packratc.Parser.~

class TestCharParser {
  def testEqual[X](p: Parser[X], str: String): Unit = {
    val res = parseString(p, str) match {
      case res @ Left(_) => assert(res.isRight)
      case Right(res, _) => res
    }
    assertEquals(str, res.toString)
  }
  
  enum Expr {
    case Symb
    case Mult(e1: Expr, e2: Expr)
    case Plus(e1: Expr, e2: Expr)

    override def toString: String = this match {
      case Symb => "a"
      case Mult(e1 : Plus, e2 : Plus) => s"($e1)*($e2)"
      case Mult(e1, e2 : Plus) => s"$e1*($e2)"
      case Mult(e1 : Plus, e2) => s"($e1)*$e2"
      case Mult(e1, e2) => s"$e1*$e2"
      case Plus(e1, e2) => s"$e1+$e2"
    }
  }

  @annotation.tailrec private def foldTree[X](cons: (X, X) => X, x: X, xs: List[X]): X = xs match {
    case Nil => x
    case y :: ys => foldTree(cons, cons(x, y), ys)
  }

  @Test def simpleExpr: Unit = {
    lazy val symb: Parser[Expr] = { char('a') <* Expr.Symb } | { 
      (char('(') ~ expr ~ char(')')) <| {
        case _ ~ e ~ _ => e
      }
    }
    lazy val mult = char('*')
    lazy val term: Parser[Expr] = { symb ~ (mult ~ symb).many } <| {
      case x ~ xs =>
        val ys = xs map (_._2)
        foldTree((a, b) => Expr.Mult(a, b), x, ys)
    }
    lazy val plus = char('+')
    lazy val expr: Parser[Expr] = { term ~ (plus ~ term).many } <| { case x ~ xs =>
      val ys = xs map (_._2)
      foldTree((a, b) => Expr.Plus(a, b), x, ys)
    }
    testEqual(expr, "a")
    testEqual(expr, "a+a*a")
    testEqual(expr, "a*a+a*(a+a)")
    testEqual(expr, "(a+a*a)*(a*a+a)")
  }
}

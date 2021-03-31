package fs2c.printing

import scala.language.implicitConversions

import fs2c.ast.c.{ Trees => C }
import C.{ BaseType => CBT }

/** Printing instances for datatypes.
  */
object printing {
  import Printer.{*, given}
  
  /** Printing instances for C-related datatypes.
    */
  object c {
    given cType: Printing[C.Type] = new Printing[C.Type] {
      def print(t: C.Type)(using printer: Printer) = t match {
        case t: CBT => baseType.print(t)
        case t: C.FuncType => funcType.print(t)
        case t: C.StructType => structType.print(t)
        case t: C.AliasType => aliasType.print(t)
        case t: C.PointerType => pointerType.print(t)
        case C.VoidType => voidType.print(C.VoidType)
      }
    }
    
    val baseType: Printing[CBT] = new Printing[CBT] {
      def print(t: CBT)(using printer: Printer) = t match {
        case CBT.IntType => printer.print("int")
        case CBT.DoubleType => printer.print("double")
        case CBT.CharType => printer.print("char")
      }
    }

    val funcType: Printing[C.FuncType] = new Printing[C.FuncType] {
      def print(t: C.FuncType)(using printer: Printer) = t match {
        case C.FuncType(retType, paramTypes) =>
          cType.print(retType)
          printer.print(" (*) (")
          paramTypes match {
            case Nil =>
            case x :: xs =>
              cType.print(x)
              xs foreach { t =>
                printer.print(", ")
                cType.print(t)
              }
          }
          printer.print(")")
      }
    }
    
    val structType: Printing[C.StructType] = new Printing[C.StructType] {
      def print(t: C.StructType)(using printer: Printer) = {
        val str = s"struct ${t.structSym.name} *"
        printer.print(str)
      }
    }

    val aliasType: Printing[C.AliasType] = new Printing[C.AliasType] {
      def print(t: C.AliasType)(using printer: Printer) = printer.print(t.aliasSym.name)
    }

    val pointerType: Printing[C.PointerType] = new Printing[C.PointerType] {
      def print(t: C.PointerType)(using printer: Printer) = {
        printer.print("(")
        cType.print(t.dealiasType)
        printer.print(")")
        printer.print(" *")
      }
    }

    val voidType: Printing[C.VoidType.type] = new Printing[C.VoidType.type] {
      def print(t: C.VoidType.type)(using printer: Printer) = printer.print("void")
    }

    /** Showing an expression.
      * 
      * Binding level of expressions.
      * 
      * -1 : Starting
      * 0  : &&, ||
      * 1  : ==, !=, >, <, >=, <=
      * 2  : +, -
      * 3  : *, /
      * 4  : ^
      * 10 : unary -, !
      * 15 : select, apply
      * 20 : Identifier
      */
    def showExpr(expr: C.Expr): String = {
      // binary operator priority
      def bopPriority: Map[C.BinOpType, Int] = Map(
        C.BinOpType.&& -> 0,
        C.BinOpType.|| -> 0,
        C.BinOpType.== -> 1,
        C.BinOpType.!= -> 1,
        C.BinOpType.>= -> 1,
        C.BinOpType.<= -> 1,
        C.BinOpType.> -> 1,
        C.BinOpType.< -> 1,
        C.BinOpType.+ -> 2,
        C.BinOpType.- -> 2,
        C.BinOpType.* -> 3,
        C.BinOpType./ -> 3,
        C.BinOpType.^ -> 4,
      )
      
      // unary operator priority
      def uopPrioriy: Map[C.UnaryOpType, Int] = Map(
        C.UnaryOpType.- -> 10,
        C.UnaryOpType.! -> 10
      )
      
      val selectAndApplyLevel: Int = 15
      val identLevel: Int = 20
      
      def go(expr: C.Expr, currentLevel: Int): String = expr match {
        case C.BinOpExpr(op, e1, e2) =>
          val nextLevel = bopPriority(op)
          val s1 = go(e1, nextLevel)
          val s2 = go(e2, nextLevel)
          val s = s"$s1 $op $s1"
          
          if nextLevel < currentLevel then
            s"($s)"
          else
            s
        case C.UnaryOpExpr(op, e) =>
          val nextLevel = uopPrioriy(op)
          val s = s"$op ${go(e, nextLevel)}"
          
          if nextLevel < currentLevel then
            s"($s)"
          else
            s
        case C.CallFunc(func, params) =>
          val sf = go(func, currentLevel = selectAndApplyLevel)
          val sparams = params map { x => go(x, currentLevel = -1) } 
          
          s"$sf(${sparams mkString ", "})"
        case C.SelectExpr(expr, designator) =>
          val s = go(expr, currentLevel = selectAndApplyLevel)
          
          s"$s.$designator"
        case C.IdentifierExpr(sym) =>
          sym.name
        case C.IntExpr(value) => value.toString
        case C.BoolExpr(value) =>
          if value then "1" else "0"
        case C.FloatExpr(value) => value.toString
      }
      
      go(expr, -1)
    }

    given expr: Printing[C.Expr] = new Printing[C.Expr] {
      def print(t: C.Expr)(using printer: Printer) = printer.print(showExpr(t))
    }
  }
}

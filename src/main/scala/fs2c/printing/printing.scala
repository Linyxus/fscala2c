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

    given expr: Printing[C.Expr] = new Printing[C.Expr] {
      def print(t: C.Expr)(using printer: Printer) = ???
    }

    val binOpExpr: Printing[C.BinOpExpr] = new Printing[C.BinOpExpr] {
      def print(t: C.BinOpExpr)(using printer: Printer) = {
        printer.inParen { expr.print(t.e1) }
        printer.print(s" ${t.op} ")
        printer.inParen { expr.print(t.e2) }
      }
    }

    val unaryOpExpr: Printing[C.UnaryOpExpr] = new Printing[C.UnaryOpExpr] {
      def print(t: C.UnaryOpExpr)(using printer: Printer) = {
        printer.print(s" ${t.op} ")
        printer.inParen { expr.print(t.e) }
      }
    }
  }
}

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
        case CBT.DoubleType => printer.print("float")
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
    
    def showPlainStructType(t: C.StructType): String = s"struct ${t.structSym.name}"
    
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
        cType.print(t.dealiasType)
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
        C.BinOpType.% -> 2,
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
          val s = s"$s1 $op $s2"

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

          s"$s->${designator.name}"
        case C.IdentifierExpr(sym) =>
          sym.name
        case C.IntExpr(value) => value.toString
        case C.BoolExpr(value) =>
          if value then "1" else "0"
        case C.FloatExpr(value) => value.toString
        case C.StringExpr(value) => s"\"$value\""
        case C.SizeOf(tp) => tp match {
          case tp: C.StructType =>
            s"sizeof(${showPlainStructType(tp)})"
          case tp =>
            s"sizeof(${tp.show})"
        }
        case C.GroundFunc(name, _) => name
        case C.CoercionExpr(tp, expr) =>
          s"(${tp.show}) ${showExpr(expr)}" match {
            case s =>
              if currentLevel == -1 then s
              else s"($s)"
          }
        case C.AddressExpr(e) => "&" ++ go(e, currentLevel)
      }

      go(expr, -1)
    }

    given expr: Printing[C.Expr] = new Printing[C.Expr] {
      def print(t: C.Expr)(using printer: Printer) = printer.print(showExpr(t))
    }

    given definition: Printing[C.Definition] = new Printing[C.Definition] {
      def print(t: C.Definition)(using printer: Printer) = t match {
        case t: C.FuncDef => funcDef.print(t)
        case t: C.TypeAliasDef => typeAliasDef.print(t)
        case t: C.StructDef => structDef.print(t)
      }
    }

    val funcDef: Printing[C.FuncDef] = new Printing[C.FuncDef] {
      def print(t: C.FuncDef)(using printer: Printer) = t match {
        case C.FuncDef(sym, _retType, params, block) =>
          def printParam(param: C.FuncParam) = param match {
            case C.FuncParam(sym, tp) =>
              cType.print(tp)
              printer.print(" ")
              printer.print(sym.name)
          }
          // print return type
          cType.print(_retType)
          // print func name
          printer.print(s" ${sym.name}(")
          // print parameter list
          params match {
            case Nil =>
            case param :: params =>
              printParam(param)
              params foreach { param =>
                printer.print(", ")
                printParam(param)
              }
          }
          printer.print(") ")
          printBlock(block, openLine = true)
      }
    }
    
    val typeAliasDef: Printing[C.TypeAliasDef] = new Printing[C.TypeAliasDef] {
      def print(t: C.TypeAliasDef)(using printer: Printer) = {
        val dealias = t.dealias
        val name = t.sym.name
        printer.print("typedef ")
        dealias match {
          case C.FuncType(retType, paramTypes) =>
            cType.print(retType)
            printer.print(" ")
            printer.inParen { printer.print(s"* $name") }
            printer.inParen { printer.printSepBy(paramTypes) { tp => cType.print(tp) } }
            printer.println(";")
          case tp =>
            cType.print(tp)
            printer.println(s" $name;")
        }
      }
    }
    
    val structDef: Printing[C.StructDef] = new Printing[C.StructDef] {
      def print(t: C.StructDef)(using printer: Printer) = t match { 
        case C.StructDef(sym, members) =>
          def printStructMember(structMember: C.StructMember) = structMember match { 
            case C.StructMember(sym, tp, struct) =>
              cType.print(tp)
              printer.println(s" ${sym.name};")
          }
          
          printer.print(s"struct ${sym.name} ")
          printer.inBlock(endLine = false) {
            members foreach printStructMember
          }
          printer.println(";")
      }
    }

    def printBlock(block: C.Block, openLine: Boolean = true)(using printer: Printer): Unit = printer.inBlock(openLine) {
      block foreach printStmt
    }

    def printStmt(stmt: C.Statement)(using printer: Printer) = stmt match {
      case C.Statement.Return(expr) =>
        val s = expr match {
          case None => ""
          case Some(expr) => " " + showExpr(expr)
        }
        printer.println("return" + s + ";")
      case C.Statement.Break => printer.println("break;")
      case C.Statement.Continue => printer.println("continue;")
      case C.Statement.If(cond, tBody, fBody) =>
        printer.print(s"if (${showExpr(cond)}) ")
        printBlock(tBody, openLine = false)
        fBody match {
          case None => printer.newLine()
          case Some(fBody) =>
            printer.print(" else ")
            printBlock(fBody, openLine = true)
        }
      case C.Statement.While(cond, body) =>
        printer.print(s"while (${showExpr(cond)}) ")
        printBlock(body, openLine = true)
      case C.Statement.Eval(expr) =>
        printer.println(s"${showExpr(expr)};")
      case C.Statement.AssignVar(d, expr) =>
        val name = d.getSym.name
        printer.println(s"$name = ${showExpr(expr)};")
      case C.Statement.AssignMember(d, designator, expr) =>
        val name = d.getSym.name
        printer.println(s"$name->${designator.name} = ${showExpr(expr)};")
      case C.Statement.Assign(p, e) =>
        printer.println(s"${showExpr(p)} = ${showExpr(e)};")
      case C.Statement.Def(C.VariableDef(sym, tp, expr, _)) =>
        val name = sym.name
        cType.print(tp)
        printer.print(s" $name")
        expr match {
          case None =>
          case Some(expr) =>
            printer.print(s" = ${showExpr(expr)}")
        }
        printer.println(";")
    }
  }
}

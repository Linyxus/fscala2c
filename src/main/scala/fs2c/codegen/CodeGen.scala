package fs2c.codegen

import fs2c.codegen.{CodeBundles => bd}
import fs2c.ast.c.{ Trees => C }
import fs2c.ast.fs.{ Trees => FS }
import FS.tpd
import FS.{ExprBinOpType => sbop}
import C.{BinOpType => cbop}
import fs2c.tools.Unique

class CodeGen {
  import CodeGen.CodeGenError

  /** Context for C code generator.
    */
  val ctx = new CodeGenContext

  def mangle(name: String): String = Unique.uniqueCName(name)

  def freshAnonFuncName: String = Unique.uniqueCName("anon_func")
  
  def freshVarName: String = Unique.uniqueCName("temp")

  private var generatedDef: List[C.Definition] = Nil

  /** Output definition. Record the definition and return it as it is.
    */
  def outDef[T <: C.Definition](d: => T): T = {
    generatedDef = d :: generatedDef
    d
  }

  def makeStructDef(name: String, memberDefs: List[(String, C.Type)]): C.StructDef = outDef {
    C.StructDef.makeStructDef(name, memberDefs)
  }

  def genExpr(expr: tpd.Expr): bd.ValueBundle = expr.tree match {
    case _ : FS.LiteralIntExpr[FS.Typed] => genIntLiteralExpr(expr.asInstanceOf)
    case _ : FS.LiteralFloatExpr[FS.Typed] => genFloatLiteralExpr(expr.asInstanceOf)
    case _ : FS.LiteralBooleanExpr[FS.Typed] => genBooleanLiteralExpr(expr.asInstanceOf)
    case _ : FS.BinOpExpr[FS.Typed] => genBinaryOpExpr(expr.asInstanceOf)
    case _ : FS.IfExpr[FS.Typed] => genIfExpr(expr.asInstanceOf)
    case _ => throw CodeGenError(s"unsupported expr $expr")
  }

  /** Generate code for int literals.
    */
  def genIntLiteralExpr(expr: tpd.LiteralIntExpr): bd.PureExprBundle = expr.assignCode { t =>
    val code = bd.PureExprBundle(C.IntExpr(t.value))
    code
  }

  /** Generate code for float literals.
    */
  def genFloatLiteralExpr(expr: tpd.LiteralFloatExpr): bd.PureExprBundle = expr.assignCode { t =>
    val code = bd.PureExprBundle(C.FloatExpr(t.value))
    code
  }

  /** Generate code for boolean literals.
    */
  def genBooleanLiteralExpr(expr: tpd.LiteralBooleanExpr): bd.PureExprBundle = expr.assignCode { t =>
    val code = bd.PureExprBundle(C.BoolExpr(t.value))
    code
  }

  def genBinaryOpExpr(expr: tpd.BinOpExpr): bd.ValueBundle = expr.assignCode { case FS.BinOpExpr(op, e1, e2) =>
    val cop = sbOp2cbOp(op)
    val bd1 = genExpr(e1)
    val bd2 = genExpr(e2)

    val cExpr = C.BinOpExpr(cop, bd1.getExpr, bd2.getExpr)
    val cBlock = bd1.getBlock ++ bd2.getBlock

    if cBlock.nonEmpty then
      bd.BlockBundle(cExpr, cBlock)
    else
      bd.PureExprBundle(cExpr)
  }

  val sbopEq = sbop.==
  val sbopNeq = sbop.!=
  def sbOp2cbOp(op: sbop): cbop = op match {
    case sbop.+ => cbop.+
    case sbop.- => cbop.-
    case sbop.* => cbop.*
    case sbop./ => cbop./
    case sbop.^ => cbop.^
    case sbop.&& => cbop.&&
    case sbop.|| => cbop.||
    case sbop.>= => cbop.>=
    case sbop.<= => cbop.<=
    case sbop.> => cbop.>
    case sbop.< => cbop.<
    case _ if op == sbopEq => cbop.==
    case _ if op == sbopNeq => cbop.!=
  }

  /** Generate C code for If expression.
    */
  def genIfExpr(expr: tpd.IfExpr): bd.BlockBundle = expr.assignCode { case FS.IfExpr(cond, et, ef) =>
    val (tempVar, tempDef) = defn.localVariable(freshVarName, C.BaseType.IntType)
    
    val bdCond = genExpr(cond)
    
    val condExpr = bdCond.getExpr
    val condBlock = bdCond.getBlock
    
    val bdt = genExpr(et)
    val bdf = genExpr(ef)
    
    val tBlock = bdt.getBlock :+ defn.assignVar(tempVar, bdt.getExpr)
    val fBlock = bdf.getBlock :+ defn.assignVar(tempVar, bdf.getExpr)
    
    val ifStmt = C.Statement.If(condExpr, tBlock, Some(fBlock))
    
    bd.BlockBundle(
      expr = C.IdentifierExpr(tempVar),
      block = tempDef ++ condBlock :+ ifStmt
    )
  }
  
  def genBlockExpr(expr: tpd.BlockExpr): bd.BlockBundle = ???
  
  def genLambdaExpr(expr: tpd.LambdaExpr, name: Option[String] = None): bd.ValueBundle = ???
}

object CodeGen {

  case class CodeGenError(msg: String) extends Exception(s"Code generation error: $msg")
  
}

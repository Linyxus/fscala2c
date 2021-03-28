package fs2c.codegen

import fs2c.codegen.{CodeBundles => bd}
import fs2c.ast.Symbol
import fs2c.ast.c.{ Trees => C }
import fs2c.ast.fs.{ Trees => FS }
import fs2c.typer.{ Types => FST }
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
  
  def freshAnonFuncTypeName: String = Unique.uniqueCName("anon_func_type")
  
  def freshVarName: String = Unique.uniqueCName("temp")

  /** Output definition. Record the definition and return it as it is.
    */
  def outDef[T <: C.Definition](d: => T): T = {
    ctx.generatedDefs = d :: ctx.generatedDefs
    d
  }

  /** Make a structure definition.
    * 
    * @param name The struct name.
    * @param memberDefs Member definitions.
    * @return
    */
  def makeStructDef(name: String, memberDefs: List[(String, C.Type)]): C.StructDef = outDef {
    C.StructDef.makeStructDef(name, memberDefs)
  }

  /** Make a type alias definition with given `name` and `dealias.`
    * ```c
    * type name = dealias;
    * ```
    * 
    * @param name The name to alias the type to.
    * @param dealias The aliased name.
    * @return Type alias definition.
    */
  def makeAliasDef(name: String, dealias: C.Type): C.TypeAliasDef = outDef {
    C.TypeAliasDef.makeTypeAliasDef(name, dealias)
  }

  /** Generate C type for Scala type.
    * 
    * @param tp The input Scala type.
    * @param aliasName Alias definition name. It will be passed if a alias needed to be created. 
    *                  See [[CodeGen.genLambdaType]].
    * @return Generated C code bundle for the given type.
    */
  def genType(tp: FST.Type, aliasName: Option[String] = None): bd.TypeBundle = tp.assignCode {
    tp match {
      case FST.GroundType.IntType => bd.SimpleTypeBundle {
        C.BaseType.IntType
      }
      case FST.GroundType.FloatType => bd.SimpleTypeBundle {
        C.BaseType.DoubleType
      }
      case FST.GroundType.BooleanType => bd.SimpleTypeBundle {
        C.BaseType.IntType
      }
      case FST.LambdaType(paramTypes, valueType) => genLambdaType(paramTypes, valueType, aliasName)
    }
  }

  /** Generate C code bundle for a LambdaType in Scala.
    */
  def genLambdaType(paramTypes: List[FST.Type], valueType: FST.Type, aliasName: Option[String]): bd.AliasTypeBundle = {
    val name = aliasName getOrElse freshAnonFuncTypeName
    
    val tps = paramTypes map { t => genType(t, None).getTp }
    val vTp = genType(valueType, None).getTp
    
    val d: C.TypeAliasDef = maybeAliasFuncType(defn.funcType(tps, vTp), name)
    
    bd.AliasTypeBundle(C.AliasType(d.sym), d)
  }

  /** Maybe generate alias definition for `funcType`.
    * It will first looks up `funcType` in cache, return the generated type alias definition if already generated,
    * output a new type alias definition otherwise.
    * 
    * @param funcType 
    * @param aliasName Name for the alias definition.
    * @return
    */
  def maybeAliasFuncType(funcType: C.FuncType, aliasName: String): C.TypeAliasDef =
    ctx.genFuncCache.get(funcType) match {
      case Some(alias) => alias
      case None => outDef {
        val res = makeAliasDef(aliasName, funcType)
        ctx.genFuncCache = ctx.genFuncCache.updated(funcType, res)
        res
      }
    }

  /** Generate C code for Scala expressions.
    */
  def genExpr(expr: tpd.Expr): bd.ValueBundle = expr.tree match {
    case _ : FS.LiteralIntExpr[FS.Typed] => genIntLiteralExpr(expr.asInstanceOf)
    case _ : FS.LiteralFloatExpr[FS.Typed] => genFloatLiteralExpr(expr.asInstanceOf)
    case _ : FS.LiteralBooleanExpr[FS.Typed] => genBooleanLiteralExpr(expr.asInstanceOf)
    case _ : FS.BinOpExpr[FS.Typed] => genBinaryOpExpr(expr.asInstanceOf)
    case _ : FS.IfExpr[FS.Typed] => genIfExpr(expr.asInstanceOf)
    case _ : FS.LambdaExpr[FS.Typed] => genLambdaExpr(expr.asInstanceOf)
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
  
  def genLambdaExpr(expr: tpd.LambdaExpr, lambdaName: Option[String] = None): bd.ValueBundle = expr.assignCode {
    case FS.LambdaExpr(params, _, body) =>
      val lambdaType = expr.tpe
      
      ???
  }
  
  def escapedVars(freeNames: List[Symbol[_]]): List[Symbol[tpd.LocalDefBind]] = {
    def recur(xs: List[Symbol[_]], acc: List[Symbol[tpd.LocalDefBind]]): List[Symbol[tpd.LocalDefBind]] = xs match {
      case Nil => Nil
      case x :: xs => x.dealias match {
        case tpt : FS.Typed[_] => tpt.tree match {
          case localDef: FS.LocalDef.Bind[_] if !localDef.isLambdaBind =>
            recur(xs, x.asInstanceOf :: acc)
          case _ => recur(xs, acc)
        }
        case _ => recur(xs, acc)
      }
    }
    
    recur(freeNames, Nil).distinctBy(eq)
  }
  
}

object CodeGen {

  case class CodeGenError(msg: String) extends Exception(s"Code generation error: $msg")
  
}

package fs2c.codegen

import fs2c.codegen.{CodeBundles => bd}
import fs2c.ast.Symbol
import fs2c.ast.c.{ Trees => C }
import fs2c.ast.c.Trees.asC
import fs2c.ast.fs.{ Trees => FS }
import fs2c.typer.{ Types => FST }
import FS.tpd
import FS.{ExprBinOpType => sbop, ExprUnaryOpType => suop}
import C.{BinOpType => cbop, UnaryOpType => cuop}
import fs2c.tools.Unique

class CodeGen {
  import CodeGen.CodeGenError

  /** Context for C code generator.
    */
  val ctx = new CodeGenContext

  /** Preloaded standard definitions.
    */
  val stdLib = new StdLibrary(this)

  /** Mangle name by adding postfix.
    */
  def mangle(name: String): String = Unique.uniqueCName(name)

  /** Generate a fresh anonymous function name.
    */
  def freshAnonFuncName: String = Unique.uniqueCName("anon_func")

  /** Generate a fresh anonymous function type name.
    */
  def freshAnonFuncTypeName: String = Unique.uniqueCName("anon_func_type")

  /** Generate a fresh variable name.
    */
  def freshVarName: String = Unique.uniqueCName("temp")

  /** Output definition. Record the definition and return it as it is.
    */
  def outDef[T <: C.Definition](d: => T): T = {
    ctx.generatedDefs = d :: ctx.generatedDefs
    d
  }

  def localAllocDef(name: String, tp: C.StructType): (Symbol[C.VariableDef], C.Block) = {
    val expr = useMalloc $$ C.SizeOf(tp)

    defn.localVariable(name, tp, Some(expr))
  }

  def maybeAllocLocalDef(name: String, tp: C.Type): (Symbol[C.VariableDef], C.Block) = {
    tp match {
      case tp: C.StructType => localAllocDef(name, tp)
      case tp => defn.localVariable(name, tp)
    }
  }

  /** Declare the usage of a ground function by adding its headers into included header list.
    * 
    * @param func The ground function to use.
    * @return The used ground function, returned as it is.
    */
  def useGroundFunc(func: C.GroundFunc): C.GroundFunc = {
    val includes = func.header
    ctx.addHeaders(includes)
    func
  }
  
  def useMalloc: C.GroundFunc = useGroundFunc(defn.GroundFuncs.malloc)

  def useScanf: C.GroundFunc = useGroundFunc(defn.GroundFuncs.scanf)

  def usePrintf: C.GroundFunc = useGroundFunc(defn.GroundFuncs.printf)

  def useSRand: C.GroundFunc = useGroundFunc(defn.GroundFuncs.srand)

  def useRand: C.GroundFunc = useGroundFunc(defn.GroundFuncs.rand)

  def useTime: C.GroundFunc = useGroundFunc(defn.GroundFuncs.time)

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
    val res = C.TypeAliasDef.makeTypeAliasDef(name, dealias)
    res
  }

  /** Make and output a function definition.
    */
  def makeFuncDef(name: String, valueType: C.Type, params: List[C.FuncParam], body: C.Block): C.FuncDef = outDef {
    C.FuncDef.makeFuncDef(name, valueType, params, body)
  }

  def makeMainFunc(mainStruct: C.StructDef, initFunc: C.FuncDef, mainFuncType: FST.LambdaType): C.FuncDef =
    makeFuncDef(
      "main", C.BaseType.IntType, Nil, {
        val (localDef, localBlock) =
          defn.localVariable("app", C.StructType(mainStruct.sym), Some(initFunc.$$()))
        val app = C.IdentifierExpr(localDef)
        val mem = mainStruct.ensureFind("main")
        val funcExpr = C.SelectExpr(app, mem.sym)
        val cFuncType = genType(mainFuncType).getTp
        val selectFunc = C.SelectExpr(funcExpr, stdLib.FuncClosure.load.ensureFind("func").sym)
        val selectEnv = C.SelectExpr(funcExpr, stdLib.FuncClosure.load.ensureFind("env").sym)
        val func = C.CoercionExpr(cFuncType, selectFunc)
        val runAppMain = C.CallFunc(func, List(selectEnv))
        val runStmt = C.Statement.Eval(runAppMain)
        val retStmt = C.Statement.Return(Some(C.IntExpr(0)))
        localBlock ++ List(runStmt, retStmt)
      }
    )

  /** Generate C type for Scala type.
    * 
    * @param tp The input Scala type.
    * @param aliasName Alias definition name. It will be passed if a alias needed to be created. 
    *                  See [[CodeGen.genLambdaType]].
    * @return Generated C code bundle for the given type.
    */
  def genType(tp: FST.Type, aliasName: Option[String] = None, lambdaValueType: Boolean = false): bd.TypeBundle = tp.assignCode {
    tp match {
      case FST.GroundType.UnitType => bd.SimpleTypeBundle(C.BaseType.IntType)
      case FST.GroundType.IntType => bd.SimpleTypeBundle {
        C.BaseType.IntType
      }
      case FST.GroundType.FloatType => bd.SimpleTypeBundle {
        C.BaseType.DoubleType
      }
      case FST.GroundType.BooleanType => bd.SimpleTypeBundle {
        C.BaseType.IntType
      }
      case FST.GroundType.StringType => bd.SimpleTypeBundle {
        C.PointerType(C.BaseType.CharType)
      }
      case _ : FST.LambdaType if lambdaValueType => lambdaClosureType
      case FST.LambdaType(paramTypes, valueType) => genLambdaType(paramTypes, valueType, aliasName)
      case clsDef: FS.ClassDef[FS.Typed] => clsDef.sym.dealias.code match {
        case bundle: bd.ClassBundle => bd.SimpleTypeBundle(C.StructType(bundle.structDef.sym))
        case bundle: bd.ClassRecBundle => bd.SimpleTypeBundle(C.StructType(bundle.structSym))
      }
      case tvar: FST.ClassTypeVariable => genType(tvar.classDef.tree)
    }
  }
  
  def lambdaClosureType: bd.SimpleTypeBundle = {
    val structDef = stdLib.FuncClosure.load
    bd.SimpleTypeBundle { C.StructType(structDef.sym) }
  }

  /** Generate C code bundle for a LambdaType in Scala.
    */
  def genLambdaType(paramTypes: List[FST.Type], valueType: FST.Type, aliasName: Option[String]): bd.AliasTypeBundle = {
    val name = aliasName getOrElse freshAnonFuncTypeName

    val tps = paramTypes map { t => genType(t, None, lambdaValueType = false).getTp }
    val vTp = genType(valueType, None).getTp
    
    val d: C.TypeAliasDef = maybeAliasFuncType(defn.funcType(defn.VoidPointer :: tps, vTp), name)
    
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
      case None => 
        val res = makeAliasDef(aliasName, funcType)
        ctx.genFuncCache = ctx.genFuncCache.updated(funcType, res)
        res
    }

  /** Generate C code for Scala class definition.
    *
    * @param clsDef
    * @return
    */
  def genClassDef(clsDef: tpd.ClassDef): bd.ClassBundle = clsDef assignCode { case FS.ClassDef(sym, params, _, members) =>
    clsDef.assignCode { _ => bd.ClassRecBundle(Symbol(sym.name + "_struct", null), null) }
    val structDef = genClassStructDef(sym, members)
    clsDef assignCode { _ => bd.ClassRecBundle(structDef.sym, Symbol("init_" + clsDef.tree.sym.name, null)) }

    bd.ClassBundle(structDef, genClassInit(sym, structDef, params, members))
  }

  def genClassStructDef(sym: Symbol[_], members: List[tpd.MemberDef]): C.StructDef = {
    val res = makeStructDef(
      name = sym.name + "_struct",
      memberDefs = members map { m =>
        val name = m.tree.sym.name
        val tp = genType(m.tpe, lambdaValueType = true)
        name -> tp.getTp
      }
    )

    // assign generated struct member to typed tree
    members foreach { m =>
      m assignCode { m =>
        val name = m.sym.name
        val cMem = res.ensureFind(name)
        bd.MemberBundle(cMem, res)
      }
    }

    res
  }

  def genClassInit(sym: Symbol[tpd.ClassDef],
                   structDef: C.StructDef,
                   params: List[Symbol[FS.LambdaParam]],
                   members: List[tpd.MemberDef]): C.FuncDef = {
    val (structValue, structBlock) =
      defn.localVariable(mangle("res"), C.StructType(structDef.sym), Some(defn.allocStruct(structDef)))

    val cParams: List[C.FuncParam] = params map { param =>
      val p = param.dealias
      val cp = genLambdaParam(p)
      p.code = bd.PureExprBundle(C.IdentifierExpr(cp.sym))
      cp
    }

    ctx.withSelfLocal(structValue, structDef) {
      val initBlock = members flatMap { member =>
        val memberName = member.tree match {
          case member: FS.MemberDef[_] => member.sym.name
        }
        val bundle: bd.ValueBundle = member.tree match { case FS.MemberDef(sym, _, _, _, body) =>
          body.tree match {
            case lambda: FS.LambdaExpr[_] =>
              genClassMethod(sym, body.asInstanceOf, structDef, C.IdentifierExpr(structValue))
            case _ => genExpr(body)
          }
        }
        val assign = defn.assignMember(structValue.dealias, structDef.ensureFind(memberName).sym, bundle.getExpr)

        bundle.getBlock :+ assign
      }

      makeFuncDef(
        "init_" + sym.name, C.StructType(structDef.sym),
        cParams,
        structBlock ++ initBlock :+ C.Statement.Return(Some(C.IdentifierExpr(structValue)))
      )
    }
  }

  def genClassMethod(sym: Symbol[_], lambda: tpd.LambdaExpr,
                     structDef: C.StructDef, structValue: C.IdentifierExpr[C.VariableDef]): bd.ClosureBundle = {
    val escaped = lambda.freeNames filter { sym =>
      sym.dealias match {
        case tpt: FS.Typed[_] => tpt.tree match {
          case t: FS.MemberDef[_] => false
          case _ => true
        }
        case _ => true
      }
    }

    // filter lambda free names
    lambda.freeNames = escaped

    genLambdaExpr(
      lambda,
      lambdaName = Some(s"${structDef.sym.name}_${sym.name}"),
      self = Some((structDef, structValue))
    )
  }

  /** Generate C code for Scala expressions.
    */
  def genExpr(expr: tpd.Expr, lambdaName: Option[String] = None): bd.ValueBundle = expr.tree match {
    case _ : FS.LiteralUnitExpr[_] => expr assignCode { _ => bd.PureExprBundle(0.asC) }
    case _ : FS.LiteralIntExpr[FS.Typed] => genIntLiteralExpr(expr.asInstanceOf)
    case _ : FS.LiteralFloatExpr[FS.Typed] => genFloatLiteralExpr(expr.asInstanceOf)
    case _ : FS.LiteralBooleanExpr[FS.Typed] => genBooleanLiteralExpr(expr.asInstanceOf)
    case _ : FS.LiteralStringExpr[_] => genStringLiteralExpr(expr.asInstanceOf)
    case _ : FS.NextRandom[_] => bd.PureExprBundle { useRand.appliedTo() }
    case _ : FS.IdentifierExpr[FS.Typed] => genIdentifierExpr(expr.asInstanceOf)
    case _ : FS.BinOpExpr[FS.Typed] => genBinaryOpExpr(expr.asInstanceOf)
    case _ : FS.UnaryOpExpr[FS.Typed] => genUnaryOpExpr(expr.asInstanceOf)
    case _ : FS.IfExpr[FS.Typed] => genIfExpr(expr.asInstanceOf)
    case _ : FS.BlockExpr[FS.Typed] => genBlockExpr(expr.asInstanceOf)
    case _ : FS.LambdaExpr[FS.Typed] => genLambdaExpr(expr.asInstanceOf, lambdaName = lambdaName)
    case _ : FS.ApplyExpr[FS.Typed] => genApplyExpr(expr.asInstanceOf)
    case _ : FS.SelectExpr[FS.Typed] => genSelectExpr(expr.asInstanceOf)
    case _ : FS.NewExpr[FS.Typed] => genNewExpr(expr.asInstanceOf)
    case _ : FS.Printf[FS.Typed] => genPrintfExpr(expr.asInstanceOf)
    case _ => throw CodeGenError(s"unsupported expr $expr")
  }

  def genPrintfExpr(expr: tpd.Printf): bd.BlockBundle = expr assignCode {
    case FS.Printf(fmt, args, false) =>
      val codes = args map { e => genExpr(e) }

      bd.BlockBundle(
        block = (codes flatMap (_.getBlock)) :+ C.Statement.Eval(usePrintf.appliedTo(fmt.asC :: codes.map(_.getExpr))),
        expr = 0.asC
      )
    case FS.Printf(fmt, args, _) =>
      val codes = args.map { e => genExpr(e) }
      val (tempVar, varDef) =
        defn.localVariable(freshVarName, C.PointerType(C.BaseType.CharType), Some(useMalloc.appliedTo(1024.asC)))
      val printBlock = List(
        C.Statement.Eval(useGroundFunc(defn.GroundFuncs.sprintf).appliedTo(tempVar.cIdent :: fmt.asC :: codes.map(_.getExpr)))
      )
      bd.BlockBundle(
        expr = tempVar.cIdent,
        block = codes.flatMap(_.getBlock) ++ varDef ++ printBlock
      )
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

  def genStringLiteralExpr(expr: tpd.LiteralStringExpr): bd.PureExprBundle = expr.assignCode { t =>
    val code = bd.PureExprBundle(C.StringExpr(t.value))
    code
  }

  /** Generate code for a Scala identifier. It will extract the corresponding C definition of
    * the referred Scala identifier. Closure environment will also be considered.
    */
  def genIdentifierExpr(expr: tpd.IdentifierExpr): bd.PureExprBundle = expr.assignCode { t =>
    t.sym match {
      case resolved : Symbol.Ref.Resolved[_] => 
        val sym = resolved.sym

        ctx.refClosureEnv(sym) match {
          case None =>
          case Some(expr) => return bd.PureExprBundle(expr)
        }
        
        sym.dealias match {
          case lambdaParam: FS.LambdaParam =>
            lambdaParam.code match {
              case bundle : bd.ValueBundle => bd.PureExprBundle(bundle.getExpr)
              case _ => assert(false, s"unexpected code bundle for lambda param: ${lambdaParam.code}")
            }
          case tpt : FS.Typed[_] => tpt.code match {
            case bundle : bd.VariableBundle =>
              bd.PureExprBundle(C.IdentifierExpr(bundle.varDef.sym))
            case bundle : bd.MemberBundle =>
              ctx.refClosureSelf(bundle.memberDef.sym) match {
                case None => ctx.refSelf(bundle.memberDef.sym) match {
                  case None =>
                    assert(false, "referencing a class member, but do not have self in context")
                  case Some(expr) => bd.PureExprBundle(expr)
                }
                case Some(expr) => bd.PureExprBundle(expr)
              }
            case _ =>
              throw CodeGenError(s"unsupported referenced typed tree: $tpt with generated code ${tpt.code}")
          }
          case x =>
            throw CodeGenError(s"unsupported reference identifier: ${t.sym} with reference ${resolved.sym.dealias}")
        }
      case _ =>
        assert(false, "encounter unresolved symbol in code generator, this is a bug.")
    }
  }

  def genSelectExpr(expr: tpd.SelectExpr): bd.PureExprBundle = expr assignCode { case FS.SelectExpr(expr, designator) =>
    expr.tpe match {
      case clsDef: FS.ClassDef[FS.Typed] =>
        val structDef: C.StructDef = clsDef.sym.dealias.code match {
          case bundle: bd.ClassRecBundle =>
            bundle.structSym.dealias
          case bundle: bd.ClassBundle =>
            bundle.structDef
        }
        val mem = structDef.ensureFind(designator)
        val cExpr = genExpr(expr).getExpr
        bd.PureExprBundle(C.SelectExpr(cExpr, mem.sym))
      case tvar: FST.ClassTypeVariable =>
        val clsDef = tvar.classDef.tree
        val structDef: C.StructDef = clsDef.sym.dealias.code match {
          case bundle: bd.ClassRecBundle =>
            bundle.structSym.dealias
          case bundle: bd.ClassBundle =>
            bundle.structDef
        }
        val mem = structDef.ensureFind(designator)
        val cExpr = genExpr(expr).getExpr
        bd.PureExprBundle(C.SelectExpr(cExpr, mem.sym))
      case _ => assert(false, s"selecting from a non-class value with type ${expr.tpe}; this is a bug in typer!")
    }
  }

  def genNewExpr(expr: tpd.NewExpr): bd.ValueBundle = expr assignCode { case FS.NewExpr(ref, params) =>
    ref match {
      case Symbol.Ref.Resolved(sym) => sym.dealias match {
        case tpt: FS.Typed[_] =>
          val initFunc = tpt.code match {
            case bundle: bd.ClassRecBundle => C.IdentifierExpr(bundle.initSym)
            case bundle: bd.ClassBundle => C.IdentifierExpr(bundle.initDef.sym)
          }
          val paramBundles: List[bd.ValueBundle] = params map { x => genExpr(x) }
          val block = paramBundles flatMap { b => b.getBlock }
          bd.BlockBundle(C.CallFunc(initFunc, paramBundles map { x => x.getExpr }), block)
      }
      case _ => assert(false, "unresolved symbol at codegen phase")
    }
  }

  /** Generate code for a binary expression.
    */
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
    case sbop.% => cbop.%
    case sbop.&& => cbop.&&
    case sbop.|| => cbop.||
    case sbop.>= => cbop.>=
    case sbop.<= => cbop.<=
    case sbop.> => cbop.>
    case sbop.< => cbop.<
    case _ if op == sbopEq => cbop.==
    case _ if op == sbopNeq => cbop.!=
  }

  def genUnaryOpExpr(expr: tpd.UnaryOpExpr): bd.ValueBundle = expr assignCode { case FS.UnaryOpExpr(sop, e) =>
    val cop = suOp2cuOp(sop)

    val bundle = genExpr(e)

    val cExpr = C.UnaryOpExpr(cop, bundle.getExpr)
    if bundle.getBlock.nonEmpty then
      bd.BlockBundle(cExpr, bundle.getBlock)
    else
      bd.PureExprBundle(cExpr)
  }

  def suOp2cuOp(op: suop): cuop = op match {
    case suop.! => cuop.!
    case suop.- => cuop.-
  }

  /** Generate C code for If expression.
    */
  def genIfExpr(expr: tpd.IfExpr): bd.BlockBundle = expr.assignCode { case FS.IfExpr(cond, et, ef) =>
    val (tempVar, tempDef) = defn.localVariable(freshVarName, genType(et.tpe).getTp)
    
    val bdCond = genExpr(cond)
    
    val condExpr = bdCond.getExpr
    val condBlock = bdCond.getBlock
    
    val bdt = genExpr(et)
    val bdf = genExpr(ef)
    
    val tBlock = bdt.getBlock :+ defn.assignVar(tempVar.dealias, bdt.getExpr)
    val fBlock = bdf.getBlock :+ defn.assignVar(tempVar.dealias, bdf.getExpr)
    
    val ifStmt = C.Statement.If(condExpr, tBlock, Some(fBlock))
    
    bd.BlockBundle(
      expr = C.IdentifierExpr(tempVar),
      block = tempDef ++ condBlock :+ ifStmt
    )
  }

  /** Mangle `name` if now we are not at top level (to prevent possible naming conflicts), and do nothing if we are
    * at the top level.
    */
  def maybeMangleName(name: String): String =
    if ctx.isTopLevel then
      name
    else
      mangle(name)

  /** Generate code for block expression.
    */
  def genBlockExpr(expr: tpd.BlockExpr): bd.BlockBundle = ctx.innerLevel {
    expr.assignCode { case blockExpr : FS.BlockExpr[FS.Typed] =>
      def goRecLocalDef(localDef: tpd.LocalDef): Unit =
        if localDef.tree.isLambdaBind then
          localDef assignCode {
            case d : FS.LocalDef.Bind[FS.Typed] =>
              bd.RecBundle(sym = Symbol[C.FuncDef](maybeMangleName(d.sym.name), null))
            case _ =>
              assert(false, "a lambda binding must be a FS.LocalDef.Bind")
          }

      def genLocalDef(localDef: tpd.LocalDef): bd.CodeBundle & bd.HasBlock = localDef assignCode {
        case bind : FS.LocalDef.Bind[FS.Typed] =>
          val name: String = localDef.code match {
            case bd.RecBundle(sym) => sym.name
            case _ => maybeMangleName(bind.sym.name)
          }

          genExpr(bind.body, lambdaName = Some(name + "_lambda")) match {
            case bundle: bd.ClosureBundle =>
              val (varDef, varBlock) =
                defn.localVariable(
                  name = name,
                  tp = genType(localDef.tpe, lambdaValueType = true).getTp,
                  expr = Some(bundle.getExpr)
                )

              bd.VariableBundle(
                varDef = varDef.dealias,
                block = bundle.getBlock ++ varBlock
              )
            case bundle =>
              val (varDef, varBlock) = defn.localVariable(name, genType(localDef.tpe, lambdaValueType = true).getTp)

              val assignStmt = defn.assignVar(varDef.dealias, bundle.getExpr)

              varDef.dealias.associatedBundle = Some(bundle)

              bd.VariableBundle(
                varDef = varDef.dealias,
                block = (bundle.getBlock ++ varBlock) :+ assignStmt
              )
          }
        case eval : FS.LocalDef.Eval[FS.Typed] =>
          val b = genExpr(eval.expr)
          bd.PureBlockBundle(
            block = b.getBlock ++ b.getExpr.match {
              case e: C.CallFunc => List(C.Statement.Eval(e))
              case _ => Nil
            }
          )
        case wh : FS.LocalDef.While[FS.Typed] =>
          val condBundle: bd.ValueBundle = genExpr(wh.cond)
          val bodyBundle: bd.ValueBundle = genExpr(wh.body)

          bd.PureBlockBundle(
            block = condBundle.getBlock ++ List(C.Statement.While(
              cond = condBundle.getExpr,
              body = bodyBundle.getBlock ++ condBundle.getBlock
            ))
          )
        case assign : FS.LocalDef.AssignRef[FS.Typed] =>
          val p = genExpr(assign.ref)
          val e = genExpr(assign.expr)
          bd.PureBlockBundle(
            block = p.getBlock ++ e.getBlock :+ C.Statement.Assign(p.getExpr, e.getExpr)
          )
        case assign : FS.LocalDef.Assign[FS.Typed] =>
          val sym: Symbol[_] = assign.ref match {
            case ref : Symbol.Ref.Resolved[_] => ref.sym
            case _ =>
              assert(false, "unresolved symbol reference in typed tree. a bug!")
          }
          val bodyBd: bd.ValueBundle = genExpr(assign.expr)

          val assignStmt: C.Statement = sym.dealias match {
            case tpt : FS.Typed[_] =>
              tpt.code match {
                case bundle : bd.VariableBundle =>
                  defn.assignVar(bundle.varDef, bodyBd.getExpr)
                case bundle : bd.MemberBundle =>
                  val self = ctx.getClosureSelf.get
                  val assignee = C.SelectExpr(self, bundle.memberDef.sym)
                  C.Statement.Assign(assignee, bodyBd.getExpr)
                case bundle =>
                  throw CodeGenError(s"unexpected code bundle ${tpt.code} of assigned symbol")
              }
            case _ => assert(false, "symbol refers to untyped tree")
          }

          bd.PureBlockBundle(bodyBd.getBlock :+ assignStmt)
      }

      val localDefs = blockExpr.defs

      // assign placeholder recursive code bundle for each lambda binding
      localDefs foreach goRecLocalDef

      // translate local definitions
      val cBlockBundles = localDefs map genLocalDef

      // translated block
      val block1 = cBlockBundles flatMap { x => x.extractBlock }

      // translate the final expression
      val exprBundle: bd.ValueBundle = genExpr(blockExpr.expr)
      
      val (defBlock, otherBlock) =
        ((block1 ++ exprBundle.getBlock).groupBy {
          case _: C.Statement.Def => true
          case _ => false
        }) match { case m => (m.get(true).getOrElse(Nil), m.get(false).getOrElse(Nil)) }

      bd.BlockBundle(
        expr = exprBundle.getExpr,
        block = defBlock ++ otherBlock
      )
    }
  }

  def genApplyExpr(expr: tpd.ApplyExpr): bd.ValueBundle = expr.assignCode {
    case apply: FS.ApplyExpr[FS.Typed] =>
      def genReadFunc(cType: C.Type, fmtStr: String, newLine: Boolean = false, allocSize: Option[Int] = None): bd.ValueBundle = {
        val (tempVar, varDefBlock) =
          defn.localVariable(
            freshVarName,
            cType,
            allocSize map { allocSize => useMalloc $$ C.IntExpr(allocSize) }
          )

        val readAddr = allocSize match {
          case Some(_) => tempVar.cIdent
          case None => tempVar.cIdent.addressExpr
        }

        bd.BlockBundle(
          expr = tempVar.cIdent,
          block =
            varDefBlock ++ List(
              C.Statement.Eval(useScanf.appliedTo(C.StringExpr(fmtStr ++ { if newLine then "\\n" else "" }), readAddr))
            )
        )
      }

      def genPrintFunc(cType: C.Type, fmtStr: String, newLine: Boolean = false): bd.ValueBundle = {
        val (tempVar, varDefBlock) = defn.localVariable(freshVarName, cType)
        val argBundle: bd.ValueBundle = genExpr(apply.args.head)

        bd.BlockBundle(
          expr = tempVar.cIdent,
          block = varDefBlock ++ argBundle.getBlock ++ List(
            defn.assignVar(tempVar.dealias, argBundle.getExpr),
            C.Statement.Eval(usePrintf.appliedTo(C.StringExpr(fmtStr ++ { if newLine then "\\n" else "" }), tempVar.cIdent))
          )
        )
      }
      apply.func.tree match {
        case _: FS.ReadInt[_] => genReadFunc(C.BaseType.IntType, "%d")
        case _: FS.ReadFloat[_] => genReadFunc(C.BaseType.DoubleType, "%f")
        case _: FS.ReadStr[_] => genReadFunc(C.PointerType(C.BaseType.CharType), "%s", allocSize = Some(1024))
        case _: FS.PrintInt[_] => genPrintFunc(C.BaseType.IntType, "%d")
        case _: FS.PrintFloat[_] => genPrintFunc(C.BaseType.DoubleType, "%f")
        case _: FS.Print[_] => genPrintFunc(C.PointerType(C.BaseType.CharType), "%s")
        case _: FS.PrintLnInt[_] => genPrintFunc(C.BaseType.IntType, "%d", newLine = true)
        case _: FS.PrintLnFloat[_] => genPrintFunc(C.BaseType.DoubleType, "%f", newLine = true)
        case _: FS.PrintLn[_] => genPrintFunc(C.PointerType(C.BaseType.CharType), "%s", newLine = true)
        case _: FS.InitRandom[_] =>
          bd.BlockBundle(
            expr = C.IntExpr(0),
            block = List(C.Statement.Eval(useSRand appliedTo (useTime appliedTo 0.asC)))
          )
        case _: FS.Sqrt[_] =>
          val e: tpd.Expr = apply.args.head
          val bundle = genExpr(e)
          bd.BlockBundle(
            expr = useGroundFunc(defn.GroundFuncs.sqrt) $$ (bundle.getExpr),
            block = bundle.getBlock
          )
        case _ =>
          val funcBundle: bd.ValueBundle = genExpr(apply.func)
          val argsBundle: List[bd.ValueBundle] = apply.args map { arg => genExpr(arg) }

          val funcExpr = funcBundle.getExpr
          val funcBlock = funcBundle.getBlock
          val argBlock = argsBundle flatMap { b => b.getBlock }
          val argExpr = argsBundle map (_.getExpr)

          val selectFunc = C.SelectExpr(funcExpr, stdLib.FuncClosure.load.ensureFind("func").sym)
          val selectEnv = C.SelectExpr(funcExpr, stdLib.FuncClosure.load.ensureFind("env").sym)

          val funcType: FST.LambdaType = apply.func.tpe match {
            case tp: FST.LambdaType => tp
            case tp => assert(false, s"can not apply a non-lambda expr with type $tp, this is cause by a bug in typer")
          }

          val cFuncType = genLambdaType(funcType.paramTypes, funcType.valueType, None).tpe
          val func = C.CoercionExpr(cFuncType, selectFunc)

          val callExpr = C.CallFunc(func, params = selectEnv :: argExpr)

          bd.BlockBundle(
            expr = callExpr,
            block = funcBlock ++ argBlock
          )
      }
  }

  /** Generate code for lambda expressions. It will create a closure to lift a lambda expression with local references
    * to top level, while will generate a simple function definition if otherwise.
    * 
    * @param expr
    * @param lambdaName Optional name. Create a new name for anonymous function if not given.
    * @return
    */
  def genLambdaExpr(expr: tpd.LambdaExpr, lambdaName: Option[String] = None,
                    self: Option[(C.StructDef, C.IdentifierExpr[C.VariableDef])] = None): bd.ClosureBundle = expr.assignCode {
    case lambda : FS.LambdaExpr[FS.Typed] =>
      val funcName = lambdaName getOrElse freshAnonFuncName

      // generate function return type
      val lambdaType: FST.LambdaType = expr.tpe.asInstanceOf
      val valueType = lambdaType.valueType
      val cValueType: C.Type = genType(valueType, lambdaValueType = true).getTp

      // generate parameter definitions
      val cParams: List[C.FuncParam] = lambda.params map { sym => genLambdaParam(sym.dealias) }

      // compute escaped variables
      val escaped: List[Symbol[tpd.LocalDefBind] | Symbol[FS.LambdaParam]] = escapedVars(expr.freeNames)

      escaped match {
        case Nil if false =>
          val bodyBundle: bd.ValueBundle = genExpr(lambda.body)
          val block = bodyBundle.getBlock :+ C.Statement.Return(Some(bodyBundle.getExpr))
          val funcDef = makeFuncDef(
            funcName,
            cValueType,
            cParams,
            block
          )
          val identExpr = C.IdentifierExpr(funcDef.sym)

          bd.SimpleFuncBundle(identExpr, funcDef)
          ???
        case escaped =>
          var envMembers = escaped map { sym =>
            sym.name -> genType(sym.dealias match {
              case p : FS.LambdaParam => p.tpe
              case p : tpd.LocalDefBind => p.tpe
            }, lambdaValueType = true).getTp
          }
          var selfName = ""
          self match {
            case Some((selfDef, self)) =>
              selfName = mangle("self")
              envMembers = (selfName -> C.StructType(selfDef.sym)) :: envMembers
            case None => ctx.getClosureSelf match {
              case Some(_) =>
                val selfName = ctx.getSelfName.get
                val selfType = ctx.getSelfType.get
                envMembers = (selfName -> selfType) :: envMembers
              case None =>
            }
          }
          val funcEnv = createClosureEnv(envMembers, funcName)

          def initClosureEnv: (C.Expr, C.Block) = {
            val (tempVar, varBlock) = maybeAllocLocalDef(freshVarName, funcEnv.tp)

            val assignBlock = escaped map { sym =>
              ctx.refClosureEnv(sym) match {
                case Some(e) =>
                  val name = sym.name
                  val cMember = funcEnv.members find { m => m.sym.name == name } match {
                    case None =>
                      assert(false, "escaped variable should be found in closure env")
                    case Some(x) => x
                  }
                  defn.assignMember(tempVar.dealias, cMember.sym, e)
                case _ =>
                  sym.dealias match {
                    case lambdaParam : FS.LambdaParam =>
                      val name = lambdaParam.sym.name
                      val cMember = funcEnv.members find { m => m.sym.name == name } match {
                        case None =>
                          assert(false, "escaped variable should be found in closure env")
                        case Some(x) => x
                      }
                      defn.assignMember(tempVar.dealias, cMember.sym, lambdaParam.code.asInstanceOf[bd.PureExprBundle].expr)
                    case tptBind : tpd.LocalDefBind =>
                      tptBind.code match {
                        case bd.NoCode =>
                          throw CodeGenError(s"code $tptBind has not been generated; can not forward-reference")
                        case bd : bd.VariableBundle =>
                          val name = tptBind.tree.sym.name
                          val cMember = funcEnv.members find { m => m.sym.name == name } match {
                            case None =>
                              assert(false, "escaped variable should be found in closure env")
                            case Some(x) => x
                          }
                          defn.assignMember(tempVar.dealias, cMember.sym, C.IdentifierExpr(bd.varDef.sym))
                        case bd : bd.RecBundle[_] =>
                          val name = tptBind.tree.sym.name
                          val cMember = funcEnv.members find { m => m.sym.name == name } match {
                            case None =>
                              assert(false, "escaped variable should be found in closure env")
                            case Some(x) => x
                          }
                          defn.assignMember(tempVar.dealias, cMember.sym, C.IdentifierExpr(bd.sym))
                      }
                  }
              }
            }

            val assignSelfBlock = self match {
              case None if ctx.hasClosureSelf =>
                val selfName = ctx.getSelfName.get
                List(defn.assignMember(tempVar.dealias, funcEnv.ensureFind(selfName).sym, ctx.getClosureSelf.get))
              case None => Nil
              case Some((selfDef, self)) =>
                List(defn.assignMember(tempVar.dealias, funcEnv.ensureFind(selfName).sym, self))
            }

            C.IdentifierExpr(tempVar) -> (varBlock ++ assignBlock ++ assignSelfBlock)
          }

          def initClosure(func: C.Expr, env: C.Expr): (C.Expr, C.Block) = {
            val closureDef: C.StructDef = stdLib.FuncClosure.load

            val (tempVar, varDef) = maybeAllocLocalDef(s"${funcName}_closure", closureDef.tp)

            val block = varDef ++ List(
              defn.assignMember(tempVar.dealias, closureDef.ensureFind("func").sym, func),
              defn.assignMember(tempVar.dealias, closureDef.ensureFind("env").sym, env),
            )

              C.IdentifierExpr(tempVar) -> block
          }

          val (funcExpr, funcDef) = ctx.inClosure(escaped, funcEnv) {
            def selfWrapper[T](body: => T) = self match {
              case None => body
              case Some((selfDef, self)) => ctx.withSelf(funcEnv.ensureFind(selfName).sym, selfDef)(body)
            }

            def res =
              // get the final function param
              val cParams2 = ctx.getClosureEnvParam :: cParams
              val (envVar, tpEnvDef) = defn.localVariable(
                "my_env",
                C.StructType(funcEnv.sym),
                Some(C.IdentifierExpr(ctx.getClosureEnvParam.sym))
              )
              ctx.setClosureEnvVar(envVar.dealias)
              val bodyBundle: bd.ValueBundle = genExpr(lambda.body)
              val block = bodyBundle.getBlock :+ C.Statement.Return(Some(bodyBundle.getExpr))
              val funcDef = makeFuncDef(
                funcName,
                cValueType,
                cParams2,
                tpEnvDef ++ block
              )

              val identExpr = C.IdentifierExpr(funcDef.sym)

              (identExpr, funcDef)
            selfWrapper { res }
          }

          val (envExpr, envBlock) = initClosureEnv

          val (finalExpr, closureBlock) = initClosure(funcExpr, envExpr)

          bd.ClosureBundle(
            expr = finalExpr,
            block = envBlock ++ closureBlock,
            envStructDef = funcEnv,
            funcDef = funcDef
          )
      }
  }

  /** Translate a Scala [[fs2c.ast.fs.Trees.LambdaParam]] to C [[fs2c.ast.c.Trees.FuncParam]].
    * 
    * @param param
    * @return
    */
  def genLambdaParam(param: FS.LambdaParam): C.FuncParam = {
    val res = param match { case FS.LambdaParam(sym, tp, _) =>
      val cTp: C.Type = genType(tp, lambdaValueType = true).getTp
      
      val res = C.FuncParam(Symbol(sym.name, null), cTp)
      res.sym.dealias = res
        
      res
    }
    
    param.code = bd.PureExprBundle(C.IdentifierExpr(res.sym))
    
    res
  }

  /** Create a closure environment for function with local references.
    */
  def createClosureEnv(env: List[(String, C.Type)], funcName: String): C.StructDef = makeStructDef(
    name = funcName + "_env",
    memberDefs = env
  )

  /** Compute escaped variables from a free name list.
    */
  def escapedVars(freeNames: List[Symbol[_]]): List[Symbol[tpd.LocalDefBind] | Symbol[FS.LambdaParam]] = {
    def recur(xs: List[Symbol[_]], acc: List[Symbol[tpd.LocalDefBind] | Symbol[FS.LambdaParam]]): List[Symbol[tpd.LocalDefBind] | Symbol[FS.LambdaParam]] = xs match {
      case Nil => acc
      case x :: xs => x.dealias match {
        case tpt : FS.Typed[_] => tpt.tree match {
          case localDef: FS.LocalDef.Bind[_] =>
            recur(xs, x.asInstanceOf :: acc)
          case _ => recur(xs, acc)
        }
        case lambdaParam: FS.LambdaParam =>
          val newAcc: List[Symbol[tpd.LocalDefBind] | Symbol[FS.LambdaParam]] = x.asInstanceOf :: acc
          recur(xs, newAcc)
        case _ =>
          recur(xs, acc)
      }
    }
    
    val res = recur(freeNames, Nil)
    res.distinctBy(_.name)
  }
}

object CodeGen {

  case class CodeGenError(msg: String) extends Exception(s"Code generation error: $msg")

}

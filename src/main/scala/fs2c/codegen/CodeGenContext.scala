package fs2c.codegen

import fs2c.ast.Symbol
import fs2c.ast.c.{Trees => C}
import fs2c.core.SymbolTable

class CodeGenContext {
  
  /** All generated C definitions.
    */
  var generatedDefs: List[C.Definition] = Nil

  /** Cache for all generated type alias for C function type.
    */
  var genFuncCache: Map[C.FuncType, C.TypeAliasDef] = Map.empty

  /** Current scope for generated C definitions.
    */
  val cScope: SymbolTable = new SymbolTable

  protected var myTopLevel: Boolean = true

  /** Checks whether the codegen is at top level.
    */
  def isTopLevel: Boolean = myTopLevel

  /** Changes whether the codegen is at top level.
    */
  protected def setTopLevel(b: Boolean): Unit =
    myTopLevel = b

  /** Run a block of code at inner level.
    */
  def innerLevel[T](body: => T): T = {
    def origTopLevel = isTopLevel
    setTopLevel(false)
    val res = body
    setTopLevel(origTopLevel)
    res
  }
  
  /** Closure-conversion */
  protected var myClosureEnvParam: C.FuncParam = null
  protected var myClosureEnvVar: C.VariableDef = null
  protected var myClosureEnv: Map[Symbol[_], Symbol[C.StructMember]] = Map.empty
  protected var myClosureSelf: Symbol[C.StructMember] = null
  protected var myClosureSelfDef: C.StructDef = null

  /** Checks whether we have a closure.
    */
  def hasClosureEnv: Boolean = myClosureEnv ne null

  def hasClosureSelf: Boolean = hasClosureEnv && (myClosureSelf ne null) && (myClosureSelfDef ne null)

  /** Get the env parameter associated with the function closure.
    */
  def getClosureEnvParam: C.FuncParam = { 
    assert(myClosureEnvParam ne null, "current closure should not be null")
    myClosureEnvParam
  }

  /** Set the coerced environment variable.
    */
  def setClosureEnvVar(envVar: C.VariableDef): Unit = myClosureEnvVar = envVar

  /** Try to reference a symbol from the closure environment.
    * 
    * @param sym The symbol to be referenced.
    * @return
    */
  def refClosureEnv(sym: Symbol[_]): Option[C.Expr] =
    if !hasClosureEnv then
      None
    else myClosureEnv get sym map { sym =>
      C.SelectExpr(C.IdentifierExpr(myClosureEnvVar.sym), sym)
    }

  def refClosureSelf(sym: Symbol[_]): Option[C.Expr] =
    if !hasClosureSelf then
      None
    else {
      val self = C.SelectExpr(C.IdentifierExpr(myClosureEnvVar.sym), myClosureSelf)

      Some(C.SelectExpr(self, myClosureSelfDef.ensureFind(sym.name).sym))
    }

  /** Initialize the closure with a list of escaped symbols.
    * 
    * @param origSyms
    * @param closureEnv
    * @return
    */
  private def initClosure(origSyms: List[Symbol[_]], closureEnv: C.StructDef): (C.FuncParam, Map[Symbol[_], Symbol[C.StructMember]]) = {
    val origMapping: Map[String, Symbol[_]] = Map.from { origSyms map { sym => sym.name -> sym } }
    val env: Map[Symbol[_], Symbol[C.StructMember]] = Map.from { 
      closureEnv.members flatMap { m =>
        val name = m.sym.name
        origMapping get name match {
          case None =>
            None
          case Some(s) =>
            Some(s -> m.sym)
        }
      }
    }
    val param = C.FuncParam.makeFuncParam("func_env", defn.VoidPointer)
    
    (param, env)
  }

  /** Run a block of code inside a closure.
    * 
    * @param escaped The list of escaped symbols.
    * @param closureEnv
    * @param body The code to run.
    * @tparam T
    * @return
    */
  def inClosure[T](escaped: List[Symbol[_]], closureEnv: C.StructDef)(body: => T): T = {
    val (p, env) = initClosure(escaped, closureEnv)
    val (origP, origEnv) = (myClosureEnvParam, myClosureEnv)
    myClosureEnvParam = p
    myClosureEnv = env
    
    val res = body
    
    myClosureEnvParam = origP
    myClosureEnv = origEnv
    
    res
  }

  def withSelf[T](self: Symbol[C.StructMember], selfDef: C.StructDef)(body: => T): T = {
    val (origSelf, origDef) = (myClosureSelf, myClosureSelfDef)
    myClosureSelf = self
    myClosureSelfDef = selfDef

    val res = body

    myClosureSelf = origSelf
    myClosureSelfDef = origDef

    res
  }

  private var myIncluded: List[String] = Nil

  /** Get the list of included header files.
    * 
    * @return
    */
  def included: List[String] = myIncluded

  /** Append files to the included header list.
    * 
    * @param files
    */
  def addHeaders(files: List[String]): Unit = {
    myIncluded = myIncluded ++ files
    myIncluded = myIncluded.distinct
  }
}

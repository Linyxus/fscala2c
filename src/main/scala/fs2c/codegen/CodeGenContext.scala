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

  def isTopLevel: Boolean = myTopLevel
  
  protected def setTopLevel(b: Boolean): Unit =
    myTopLevel = b

  def innerLevel[T](body: => T): T = {
    def origTopLevel = isTopLevel
    setTopLevel(false)
    val res = body
    setTopLevel(origTopLevel)
    res
  }
  
  /** Closure-conversion */
  protected var myClosureEnvParam: C.FuncParam = null
  protected var myClosureEnv: Map[Symbol[_], Symbol[C.StructMember]] = Map.empty
  
  def hasClosureEnv: Boolean = myClosureEnv ne null
  
  def getClosureEnvParam: C.FuncParam = { 
    assert(myClosureEnvParam ne null, "current closure should not be null")
    myClosureEnvParam
  }

  def refClosureEnv(sym: Symbol[_]): Option[C.Expr] =
    if !hasClosureEnv then
      None
    else myClosureEnv get sym map { sym =>
      C.SelectExpr(C.IdentifierExpr(myClosureEnvParam.sym), sym)
    }
    
  private def initClosure(origSyms: List[Symbol[_]], closureEnv: C.StructDef): (C.FuncParam, Map[Symbol[_], Symbol[C.StructMember]]) = {
    val origMapping: Map[String, Symbol[_]] = Map.from { origSyms map { sym => sym.name -> sym } }
    val env: Map[Symbol[_], Symbol[C.StructMember]] = Map.from { 
      closureEnv.members map { m => 
        val name = m.sym.name
        origMapping get name match {
          case None =>
            assert(false, "name in closure env should be found in escaped variables")
          case Some(s) =>
            s -> m.sym
        }
      }
    }
    val param = C.FuncParam.makeFuncParam("func_env", C.StructType(closureEnv.sym))
    
    (param, env)
  }
  
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
}

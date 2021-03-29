package fs2c.codegen

import fs2c.ast.fs.{ Trees => FS }
import fs2c.ast.c.{ Trees => C }
import fs2c.ast.Symbol

object CodeBundles {
  
  /** Bundle of C code generated when translating a Featherweight Scala definition or expression.
    */
  trait CodeBundle
  
  trait ValueBundle extends CodeBundle {
    def getExpr: C.Expr
    def getBlock: C.Block
  }
  
  trait LambdaBundle extends ValueBundle
  
  case object NoCode extends CodeBundle
  
  case class RecBundle[T](sym: Symbol[T]) extends ValueBundle {
    override def getExpr = C.IdentifierExpr(sym)
    override def getBlock = Nil
  }

  /** Bundle of code consisting purely of a expression.
    * 
    * Translating simple arithmetic and logical expression, function application and selection
    * may result in [[PureExprBundle]].
    */
  case class PureExprBundle(expr: C.Expr) extends ValueBundle {
    override def getExpr = expr
    override def getBlock = Nil
  }

  /** Bundle of code consisting of a block of C statements and results a C expression.
    * 
    * For example, the following If expression in Scala will result in a [[BlockBundle]].
    * Scala code:
    * ```scala
    * if x > 0 then
    *   1
    * else
    *   -1
    * ```
    * Translated C code:
    * ```c
    * int t;
    * if (x > 0) {
    *   t = 1;
    * } else {
    *   t = -1;
    * }
    * ```
    * The resulted expression in the bundle will be `t`, which is a temp variable.
    */
  case class BlockBundle(expr: C.Expr, block: C.Block) extends CodeBundle with ValueBundle {
    override def getExpr = expr
    override def getBlock = block
  }

  /** Code bundle for simple lifted local lambda functions without non-local references (free variables).
    */
  case class SimpleFuncBundle(expr: C.Expr, funcDef: C.FuncDef) extends LambdaBundle {
    override def getExpr = expr
    override def getBlock = Nil
  }

  /** Bundle of code consisting of a lambda closure.
    * 
    * For example, for a local lambda with free variables:
    * ```scala
    * val adder = (i : Int) => n + i
    * ```
    * where `n : Int` is a free variable.
    * 
    * It will be translated into a group of C definitions:
    * ```c
    * struct adder_env {
    *   int n;
    * }
    * 
    * int adder(struct adder_env *env, int i) {
    *   return env->n + i;
    * }
    * 
    * typedef int (*adder_t)(struct adder_env *, int)
    * ```
    * and a block of statements:
    * ```
    *   struct adder_env *env = init_adder_env(n);
    *   struct func_closure *adder_closure = init_func_closure(adder, env);
    * ```
    * and finally the expression `adder_closure`.
    * 
    * To better illustrate the idea of function closures, at call sites of `adder` (maybe outside the scope where
    * `adder` is defined), the generated C code will become:
    * ```c
    * (adder_t*)(adder_closure->func)((struct adder_env *)(adder_closure->env), i)
    * ```
    */
  case class ClosureBundle(expr: C.Expr, 
                           block: C.Block,
                           envStructDef: C.StructDef,
                           funcDef: C.FuncDef,
                          ) extends LambdaBundle {
    override def getExpr = expr
    override def getBlock = block
  }

  /** Code bundle consisting of the C structure definition, related method function definition of a Scala class.
    * 
    * A simple example:
    * ```scala
    * class Point(x0 : Int, y0 : Int) {
    *   val x = x0
    *   val y = y0
    *   val distTo = (that : Point) => {
    *     val abs = (n : Int) => if n < 0 then -n else n
    *     val dx = abs(that.x - x)
    *     val dy = abs(that.y - y)
    *     dx + dy
    *   }
    * }
    * ```
    * The above snippet will be translated to
    * ```c
    * // struct
    * struct Point {
    *   int x;
    *   int y;
    * }
    * 
    * // init function
    * struct Point *init_Point(int x0, int y0) {
    *   // ...
    * }
    * 
    * // lifted local lambda
    * // no env provided, no closure is created, since this lambda have no free variables
    * int Point_distTo_abs(int n) {
    *   int t;
    *   if (n < 0) {
    *     t = -n;
    *   else {
    *     t = n;
    *   }
    *   return t;
    * }
    * 
    * // class method
    * int Point_distTo(struct Point *this, struct Point *that) {
    *   int dx = Point_distTo_abs(that->x - this->x);
    *   int dy = Point_distTo_abs(that->y - this->y);
    *   dx + dy
    * }
    * ```
    */
  case class ClassBundle(structDef: C.StructDef, initDef: C.FuncDef, methodsDef: List[C.FuncDef]) extends CodeBundle

  /** Code bundle produced when generatin code for Scala types.
    */
  trait TypeBundle extends CodeBundle {
    def getTp: C.Type
    
    def getDef: Option[C.Definition]
  }

  /** A simple type bundle from one-to-one correspondance between Scala and C types.
    * 
    * @param tpe Corresponding C type.
    */
  case class SimpleTypeBundle(tpe: C.Type) extends TypeBundle {
    override def getTp = tpe
    override def getDef = None
  }

  /** A aliased type bundle with C alias type definition to simplify code representation.
    * 
    * @param tpe Alias type.
    * @param aliasDef Associated C type alias definition.
    */
  case class AliasTypeBundle(tpe: C.AliasType, aliasDef: C.TypeAliasDef) extends TypeBundle {
    override def getTp = tpe
    override def getDef = Some(aliasDef)
  }
  
  case class VariableBundle(varDef: C.VariableDef, block: C.Block) extends CodeBundle

}

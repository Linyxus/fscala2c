package fs2c.parser

import scala.language.implicitConversions
import fs2c.io.ScalaSource
import fs2c.tokenizer.{ScalaToken, ScalaTokenType, Tokenizer}
import fs2c.tools.packratc.scala_token.ScalaTokenParser.{given, _}
import fs2c.tools.packratc.scala_token.*
import fs2c.tools.packratc.Parser.~
import fs2c.ast.fs.Trees
import Trees.{untpd, Untyped, Typed}
import fs2c.ast.Symbol
import fs2c.typer.Types
import Types.*

/** Parser for Featherweight Scala.
  */
class ScalaParser {
  
  /** Records syntax error while parsing.
    *
    * @param token The token where this error is found.
    * @param msg The error message.
    */
  case class SyntaxError(token: Option[ScalaToken], msg: String) extends Exception

  /** Scope of parsing.
    *
    * @param syms   Symbols contained in the scope.
    * @param parent Parent scope of the current one. None if this is the root scope.
    */
  case class ParseScope(var syms: Map[String, Symbol[_]], parent: ParseScope)

  /** Current scope.
    * Initially, it will be an empty root scope.
    */
  var currentScope: ParseScope = ParseScope(Map.empty, null)

  /** Enter a new scope.
    */
  def locateScope(): Unit = {
    currentScope = ParseScope(Map.empty, currentScope)
    symCacheStack = symCacheStack.head :: symCacheStack
  }

  /** Exit the current scope.
    */
  def relocateScope(): Unit = {
    currentScope = currentScope.parent.ensuring(_ != null, "can not relocate from the root scope.")
    symCacheStack = symCacheStack.tail
  }

  /** Find symbol in the given scope. Will not search deeper into its parent scope.
    * 
    * @param scope Scope to search in.
    * @param symName Symbol name to look for.
    */
  def findSymIn(scope: ParseScope, symName: String): Option[Symbol[_]] = scope.syms get symName

  /** Find symbol from all nested scopes.
    * 
    * @param symName Symbol name to look for.
    */
  def findSym(symName: String): Option[Symbol[_]] = {
    @annotation.tailrec def recur(scope: ParseScope): Option[Symbol[_]] = scope match {
      case null => None
      case _ => findSymIn(scope, symName) match {
        case Some(sym) => Some(sym)
        case None => recur(scope.parent)
      }
    }
    
    recur(currentScope)
  }

  /** Find symbol in the current scope.
    */
  def findSymHere(symName: String): Option[Symbol[_]] = findSymIn(currentScope, symName)

  /** Add a symbol into the current scope.
    */
  def addSymbol(sym: Symbol[_]): Unit = 
    currentScope.syms = currentScope.syms.updated(sym.name, sym)
  
  /** A cache for symbol names. 
    * Always store the _closest_ symbol (in the newest scope) for each name.
    */
  def symCache: Map[String, Symbol[_]] = symCacheStack.head

  /** A stack for symbol name cache.
    * The stack grows when the parser enters a new scope, 
    * and will trackback to previous state when the parser exits a scope.
    */
  var symCacheStack: List[Map[String, Symbol[_]]] = List(Map.empty)

  /** Parser for expressions.
    */
  lazy val exprParser: Parser[untpd.Expr] = ???

  /** Parser for lambda expressions.
    */
  lazy val lambdaExpr: Parser[untpd.LambdaExpr] = {
    val param: Parser[(ScalaToken, Type)] = (identifier ~ ":" ~ typeParser) <| { case n ~ _ ~ t => (n, t) }
    val params: Parser[List[Symbol[Trees.LambdaParam]]] = param.sepBy(",").wrappedBy("(", ")") <| { ps => 
      // create a new scope for the lambda
      locateScope()
      def recur(ps: List[(ScalaToken, Type)]): List[Symbol[Trees.LambdaParam]] = ps match {
        case Nil => Nil
        case (tk @ ScalaToken(_, _, ScalaTokenType.Identifier(name)), tpe) :: ps =>
          if findSymHere(name).isDefined then
            throw SyntaxError(Some(tk), s"duplicated parameter name in lambda definition: $name")
          else {
            val lambdaParam: Trees.LambdaParam = Trees.LambdaParam(Symbol(name, null), tpe)
            lambdaParam.sym.dealias = lambdaParam
            lambdaParam.sym :: recur(ps)
          }
        case _ :: ps => recur(ps)
      }
      recur(ps)
    }
    val body: Parser[untpd.Expr] = blockExpr <| { x => 
      // exit the scope
      relocateScope()
      Untyped(x.tree) 
    }
    
    (params ~ "=>" ~ body) <| { case params ~ _ ~ body => Untyped(Trees.LambdaExpr(params, None, body)) }
  }

  /** Parser for block expressions.
    */
  lazy val blockExpr: Parser[untpd.BlockExpr] = ???

  /** Parser for identifiers in the expression.
    */
  lazy val identifierExprParser: Parser[untpd.IdentifierExpr] = identifier <| {
    case ScalaToken(_, _, ScalaTokenType.Identifier(symName)) =>
      symCache get (symName) match {
        case None => Untyped(Trees.IdentifierExpr[Untyped](Symbol.Ref.Unresolved(symName)))
        case Some(sym) => Untyped(Trees.IdentifierExpr[Untyped](Symbol.Ref.Resolved(sym)))
      }
  }

  /** Parser for types, and is an alias for LambdaType parser. For more information, see [[lambdaType]] Parser.
    */
  lazy val typeParser: Parser[Type] = lambdaType

  /** Parser for lambda types.
    * 
    * For example:
    * ```scala
    * (Int, Int) => Int => Int
    * ```
    * will be parsed into
    * ```
    * LambdaType(List(Int, Int), LambdaType(Int, Int))
    * ```
    */
  lazy val lambdaType: Parser[Type] = (typeTerm &! "=>") or {
    val l: Parser[List[Type]] = typeParser.sepBy(",").wrappedBy("(", ")")
    val t: Parser[List[Type]] = (typeTerm <| { x => x :: Nil }) | l
    
    (t ~ "=>" ~ lambdaType) <| { case argTpe ~ _ ~ bodyTpe => LambdaType(argTpe, bodyTpe) }
  }
  lazy val typeTerm: Parser[Type] = groundType or { typeParser.wrappedBy("(", ")") }
  lazy val groundType: Parser[GroundType] = (symbol("Int") <* GroundType.IntType) or
      (symbol("Float") <* GroundType.FloatType) or
      (symbol("Boolean") <* GroundType.BooleanType) or
      arrayType
  lazy val arrayType: Parser[GroundType.ArrayType] =
    (symbol("Array") seq "[" seq typeParser seq "]") <| {
      case _ ~ _ ~ tpe ~ _ => GroundType.ArrayType(tpe)
    }
}

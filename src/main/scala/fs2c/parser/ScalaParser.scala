package fs2c.parser

import scala.language.implicitConversions
import fs2c.io.ScalaSource
import fs2c.tokenizer.{ScalaToken, ScalaTokenType, Tokenizer}
import fs2c.tools.packratc.scala_token.ScalaTokenParser.{given, _}
import fs2c.tools.packratc.scala_token.*
import fs2c.tools.packratc.Parser.{~}
import fs2c.ast.fs.Trees
import Trees.{untpd, Untyped, Typed}
import fs2c.ast.Symbol
import fs2c.typer.Types
import Types.*

/** Parser for Featherweight Scala token stream.
  *
  * It will not only parse source tokens into ASTs, but also resolve symbol references, and
  * produce untyped trees. All symbols will be resolved in an untyped tree, and unknown references
  * will result in a [[SyntaxError]].
  */
class ScalaParser {

  /** Records syntax error while parsing.
    *
    * @param token The token where this error is found.
    * @param msg The error message.
    */
  case class SyntaxError(token: Option[ScalaToken], msg: String) extends Exception(msg)

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
  def addSymbol(sym: Symbol[_]): Unit = {
    currentScope.syms = currentScope.syms.updated(sym.name, sym)
  }

  /** A cache for symbol names.
    * Always store the _closest_ symbol (in the newest scope) for each name.
    */
//  def symCache: Map[String, Symbol[_]] = symCacheStack.head

  /** A stack for symbol name cache.
    * The stack grows when the parser enters a new scope,
    * and will trackback to previous state when the parser exits a scope.
    */
  var symCacheStack: List[Map[String, Symbol[_]]] = List(Map.empty)

  import fs2c.tools.packratc.ParserFunctions.ExpressionParser
  import ExpressionParser._
  import OpAssoc._
  import OpInfo._
  import Trees.{ ExprBinOpType => bop, ExprUnaryOpType => uop }
  /** Parser for expressions.
    *
    * Small (fixed) grammar for expression:
    * 
    * ```
    * expr   -> logic
    * logic  -> rel ( ( '&&' | '||' ) rel )*
    * rel    -> item ( ( '>' | '<' | '>=' | '<=' | '==' ) item )*
    * item   -> factor ( ( '+' | '-' ) factor )*
    * factor -> exp ( ( '*' | '/' ) exp )*
    * exp    -> unary ( '^' unary )*
    * unary  -> [( '!' | '-' )] app
    * app    -> term ( '(' [ expr, ( ',' expr )* ] ')' )*
    * term   -> identifier | lambda | block | '(' expr ')'
    * ```
    *
    */
  def exprParser: Parser[untpd.Expr] = {
    makeExprParser(List(
      Binary(LeftAssoc, List(
        "&&" <* { (e1, e2) => Untyped(Trees.BinOpExpr(bop.&&, e1, e2)) },
        "||" <* { (e1, e2) => Untyped(Trees.BinOpExpr(bop.&&, e1, e2)) },
      )),
      Binary(LeftAssoc, List(
        ">" <* { (e1, e2) => Untyped(Trees.BinOpExpr(bop.>, e1, e2)) },
        "<" <* { (e1, e2) => Untyped(Trees.BinOpExpr(bop.<, e1, e2)) },
        ">=" <* { (e1, e2) => Untyped(Trees.BinOpExpr(bop.>=, e1, e2)) },
        "<=" <* { (e1, e2) => Untyped(Trees.BinOpExpr(bop.<=, e1, e2)) },
      )),
      Binary(LeftAssoc, List(
        "+" <* { (e1, e2) => Untyped(Trees.BinOpExpr(bop.+, e1, e2)) },
        "-" <* { (e1, e2) => Untyped(Trees.BinOpExpr(bop.-, e1, e2)) },
      )),
      Binary(LeftAssoc, List(
        "*" <* { (e1, e2) => Untyped(Trees.BinOpExpr(bop.*, e1, e2)) },
        "/" <* { (e1, e2) => Untyped(Trees.BinOpExpr(bop./, e1, e2)) },
      )),
      Binary(LeftAssoc, List(
        "^" <* { (e1, e2) => Untyped(Trees.BinOpExpr(bop.^, e1, e2)) },
      )),
      Unary(List(
        "!" <* { (e) => Untyped(Trees.UnaryOpExpr(uop.!, e)) },
        "-" <* { (e) => Untyped(Trees.UnaryOpExpr(uop.-, e)) },
      ))
    ), applyAndSelectExpr)
  }

  /** Parses the following grammar corresponding to member selection and application in the language.
    * ```
    * applyOrSelect ::= applyOrSelect '.' identifier | applyOrSelect '(' param_list ')' | term
    * ```
    * It is a left-recursive grammar, and can be transformed into:
    * ```
    * applyOrSelect ::= term ( ( '.' identifier ) | ( '(' param_list ')' ) )*
    * ```
    */
  def applyAndSelectExpr: Parser[untpd.Expr] = {
    def selP: Parser[untpd.Expr => untpd.Expr] = 
      { "." ~ identifier } <| { case _ ~ ScalaToken(_, _, ScalaTokenType.Identifier(member)) =>
        e => Untyped(Trees.SelectExpr(e, Symbol.Ref.Unresolved(member)))
      }
    def applyP: Parser[untpd.Expr => untpd.Expr] =
      exprParser.sepBy(",").wrappedBy("(", ")") <| { case es =>
        e => Untyped(Trees.ApplyExpr(e, es))
      }

    (exprTerm ~ (selP or applyP).many) <| { case e ~ ts =>
      ts.foldLeft(e) { (expr, func) => func(expr) }
    }
  }

  /** Parses a term in the expression grammar.
    */
  def exprTerm: Parser[untpd.Expr] = identifierExpr or { lambdaExpr or blockExpr or exprParser.wrappedBy("(", ")") }

  /** Parser for lambda expressions.
    */
  def lambdaExpr: Parser[untpd.Expr] = {
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
            addSymbol(lambdaParam.sym)
            lambdaParam.sym :: recur(ps)
          }
        case _ :: ps => recur(ps)
      }
      recur(ps)
    }
    val body: Parser[untpd.Expr] = exprParser <| { x =>
      // exit the scope
      relocateScope()
      Untyped(x.tree)
    }

    (params ~ "=>" ~ body) <| { case params ~ _ ~ body => Untyped(Trees.LambdaExpr(params, None, body)) }
  }

  /** Parser for block expressions.
    */
  def blockExpr: Parser[untpd.BlockExpr] = {
    val line: Parser[untpd.LocalDef] = localDef << NL
    val begin: Parser[ScalaToken] = ("{" ~ blockStart ~ NL) <| { case t ~ _ ~ _ => locateScope(); t }
    val end: Parser[ScalaToken] = ("}" ~ blockEnd) <| { case t ~ _ => relocateScope(); t }
    val block: Parser[untpd.BlockExpr] = 
      (begin ~ line.many ~ end) <| { case beginToken ~ ls ~ endToken => 
        ls match {
          case Nil => throw SyntaxError(Some(endToken), s"Block expression should not be empty")
          case ls : List[untpd.LocalDef] =>
            val lastOne: Trees.LocalDef[Untyped] = ls.last.tree
            lastOne match {
              case eval: fs2c.ast.fs.Trees.LocalDef.Eval[Untyped] =>
                Untyped(Trees.BlockExpr(ls.init, eval.expr))
              case _ =>
                throw SyntaxError(Some(endToken), s"Expecting a expression at the end of a block.")
            }
        }
      }

    block
  }

  def localDef: Parser[untpd.LocalDef] = localDefBind | localDefAssign | localDefEval

  def localDefBind: Parser[untpd.LocalDef] = {
    val kw: Parser[ScalaTokenType] = ("val" | "var") <| { case ScalaToken(_, _, t) => t }
    val ascription: Parser[Option[Type]] = (":" >> typeParser).optional
    (kw ~ identifier ~ ascription ~ "=" ~ exprParser) <| { 
      case bindKw ~ (t @ ScalaToken(_, _, ScalaTokenType.Identifier(name))) ~ tpe ~ _ ~ body =>
        val mutable = bindKw match {
          case ScalaTokenType.KeywordVal => false
          case ScalaTokenType.KeywordVar => true
          case _ => false
        }

        if findSymHere(name).isDefined then
          throw SyntaxError(Some(t), s"duplicated value name in local definition: $name")
        else {
          val bind: untpd.LocalDefBind = Untyped(Trees.LocalDef.Bind(Symbol(name, null), mutable, tpe, body))
          bind.tree.sym.dealias = bind
          addSymbol(bind.tree.sym)
          bind
        }
    }
  }
  
  def localDefAssign: Parser[untpd.LocalDef] = {
    (identifier ~ "=" ~ exprParser) <| { case t@ScalaToken(_, _, ScalaTokenType.Identifier(symName)) ~ _ ~ expr =>
      Untyped(Trees.LocalDef.Assign(tryResolveSymbol(symName), expr))
    }
  }

  def localDefEval: Parser[untpd.LocalDef] = exprParser <| { (expr: untpd.Expr) =>
    Untyped(Trees.LocalDef.Eval(expr))
  }
  
  def tryResolveSymbol(symName: String): Symbol.Ref = findSym(symName) match {
    case None => Symbol.Ref.Unresolved(symName)
    case Some(sym) => Symbol.Ref.Resolved(sym)
  }

  /** Parser for identifiers in the expression.
    */
  def identifierExpr: Parser[untpd.IdentifierExpr] = {
    identifier <| {
      case ScalaToken(_, _, ScalaTokenType.Identifier(symName)) =>
        findSym(symName) match {
          case None => Untyped(Trees.IdentifierExpr[Untyped](Symbol.Ref.Unresolved(symName)))
          case Some(sym) => Untyped(Trees.IdentifierExpr[Untyped](Symbol.Ref.Resolved(sym)))
        }
    }
  }

  /** Parser for types, and is an alias for LambdaType parser. For more information, see [[lambdaType]] Parser.
    */
  def typeParser: Parser[Type] = lambdaType

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
  def lambdaType: Parser[Type] = (typeTerm &! "=>") or {
    val l: Parser[List[Type]] = typeParser.sepBy(",").wrappedBy("(", ")")
    val t: Parser[List[Type]] = (typeTerm <| { x => x :: Nil }) | l

    (t ~ "=>" ~ lambdaType) <| { case argTpe ~ _ ~ bodyTpe => LambdaType(argTpe, bodyTpe) }
  }
  def typeTerm: Parser[Type] = groundType or { typeParser.wrappedBy("(", ")") }
  def groundType: Parser[GroundType] = (symbol("Int") <* GroundType.IntType) or
      (symbol("Float") <* GroundType.FloatType) or
      (symbol("Boolean") <* GroundType.BooleanType) or
      (symbol("String") <* GroundType.StringType) or
      arrayType
  def arrayType: Parser[GroundType.ArrayType] =
    (symbol("Array") seq "[" seq typeParser seq "]") <| {
      case _ ~ _ ~ tpe ~ _ => GroundType.ArrayType(tpe)
    }
}

object ScalaParser {
  /** --- debug --- */
  def parseString[X](p: Parser[X], str: String): Result[X] = {
    val tokenizer = new Tokenizer(new ScalaSource("test", str))
    parseWithTokenizer(p, tokenizer)
  }
}

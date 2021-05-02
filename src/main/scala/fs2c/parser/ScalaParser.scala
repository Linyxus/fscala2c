package fs2c.parser

import scala.language.implicitConversions
import fs2c.io.{ScalaSource, Positional}
import fs2c.tokenizer.{ScalaToken, ScalaTokenType, Tokenizer}
import fs2c.tools.packratc.scala_token.ScalaTokenParser.{given, _}
import fs2c.tools.packratc.scala_token.*
import fs2c.tools.packratc.Parser.{~}
import fs2c.ast.fs.Trees
import Trees.{untpd, Untyped, Typed, groundValueMap}
import fs2c.ast.Symbol
import fs2c.ast.Scopes._
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
    * @param msg The error message.
    */
  case class SyntaxError(msg: String) extends Exception with Positional {
    override type This = SyntaxError

    override def toString: String = showInSourceLine ++ s"\nSyntax error: $msg"
  }
  
  val scopeCtx: ScopeContext = new ScopeContext

  /** Entry parser to parse Featherweight Scala source.
    * 
    * A source file will consist of several [[Trees.ClassDef]]s.
    */
  def mainParser: Parser[List[untpd.ClassDef]] = 
    classDefParser.sepBy(NL)
  
  def fileParser: Parser[List[untpd.ClassDef]] =
    mainParser << EOF

  /** Parses class definition.
    */
  def classDefParser: Parser[untpd.ClassDef] = {
    val member: Parser[untpd.MemberDef] = memberDef << NL
    val bodyBegin: Parser[ScalaToken] = "{"
    val bodyEnd: Parser[ScalaToken] = "}" <| { t => scopeCtx.relocateScope(); t }
    val body: Parser[List[untpd.MemberDef]] =
      (bodyBegin ~ member.many ~ bodyEnd) <| { case _ ~ ls ~ _ => ls}
    val param: Parser[(ScalaToken, String, Type)] =
      (identifier ~ ":" ~ typeParser) <| { case (t @ ScalaToken(ScalaTokenType.Identifier(symName))) ~ _ ~ tpe =>
        (t, symName, tpe)
      }
    val paramList: Parser[List[Symbol[Trees.LambdaParam]]] = param.sepBy(",").wrappedBy("(", ")").optional <| {
      case None => 
        scopeCtx.locateScope()
        Nil
      case Some(xs) =>
        scopeCtx.locateScope()
        xs map { case (t, symName, tpe) =>
          if scopeCtx.findSymHere(symName).isDefined then
            throw SyntaxError(s"duplicated parameter name in class definition: $symName").withPos(t.pos)
          else {
            val lambdaParam: Trees.LambdaParam = Trees.LambdaParam(Symbol(symName, null), tpe)
            lambdaParam.sym.dealias = lambdaParam
            scopeCtx.addSymbol(lambdaParam.sym)
            lambdaParam.sym
          }
        }
    }
    val inheritance: Parser[Option[Symbol.Ref]] = { ("extends" >> identifier) <| { 
      case tk @ ScalaToken(ScalaTokenType.Identifier(symName)) =>
        tryResolveSymbol(symName)
      } 
    }.optional

    ("class" ~ identifier ~ paramList ~ inheritance ~ body) <| {
      case _ ~ (tk @ ScalaToken(ScalaTokenType.Identifier(symName))) ~ params ~ parent ~ members =>
        if scopeCtx.findSymHere(symName).isDefined then
          throw SyntaxError("duplicated class name: $symName").withPos(tk.pos)
        else {
          val ret: untpd.ClassDef = Untyped(Trees.ClassDef(Symbol(symName, null), params, parent, members))
          ret.tree.sym.dealias = ret
          ret.tree.members foreach { member => member.tree.classDef = ret.tree.sym }
          ret
        }
    }
  }
  
  def memberDef: Parser[untpd.MemberDef] = {
    val kw: Parser[ScalaTokenType] = ("val" | "var") <| { case ScalaToken(t) => t }
    val ascription: Parser[Option[Type]] = (":" >> typeParser).optional
    (kw ~ identifier ~ ascription ~ "=" ~ exprParser) <| {
      case bindKw ~ (t @ ScalaToken(ScalaTokenType.Identifier(name))) ~ tpe ~ _ ~ body =>
        val mutable = bindKw match {
          case ScalaTokenType.KeywordVal => false
          case ScalaTokenType.KeywordVar => true
          case _ => false
        }

        if scopeCtx.findSymHere(name).isDefined then
          throw SyntaxError(s"duplicated member name in class definition: $name").withPos(t.pos)
        else {
          val member: untpd.MemberDef = Untyped(Trees.MemberDef(Symbol(name, null), null, mutable, tpe, body))
          member.tree.sym.dealias = member
          scopeCtx.addSymbol(member.tree.sym)
          member
        }
    }
  }

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
  def exprParser: Parser[untpd.Expr] = ifExpr or newExpr or {
    makeExprParser(List(
      Binary(LeftAssoc, List(
        "&&" <* { (e1, e2) => Untyped(Trees.BinOpExpr(bop.&&, e1, e2)) },
        "||" <* { (e1, e2) => Untyped(Trees.BinOpExpr(bop.||, e1, e2)) },
      )),
      Binary(LeftAssoc, List(
        ">" <* { (e1, e2) => Untyped(Trees.BinOpExpr(bop.>, e1, e2)) },
        "<" <* { (e1, e2) => Untyped(Trees.BinOpExpr(bop.<, e1, e2)) },
        ">=" <* { (e1, e2) => Untyped(Trees.BinOpExpr(bop.>=, e1, e2)) },
        "<=" <* { (e1, e2) => Untyped(Trees.BinOpExpr(bop.<=, e1, e2)) },
        "==" <* { (e1, e2) => Untyped(Trees.BinOpExpr(bop.==, e1, e2)) },
        "!=" <* { (e1, e2) => Untyped(Trees.BinOpExpr(bop.!=, e1, e2)) },
      )),
      Binary(LeftAssoc, List(
        "+" <* { (e1, e2) => Untyped(Trees.BinOpExpr(bop.+, e1, e2)) },
        "-" <* { (e1, e2) => Untyped(Trees.BinOpExpr(bop.-, e1, e2)) },
      )),
      Binary(LeftAssoc, List(
        "*" <* { (e1, e2) => Untyped(Trees.BinOpExpr(bop.*, e1, e2)) },
        "/" <* { (e1, e2) => Untyped(Trees.BinOpExpr(bop./, e1, e2)) },
        "%" <* { (e1, e2) => Untyped(Trees.BinOpExpr(bop.%, e1, e2)) },
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

  /** Parses if expressions. Logical new line before then is valid despite within the same expression. Specifically,
    * the following example is valid.
    * 
    * ```scala
    * if n == 0 then
    *   1
    * else
    *   n * fact(n - 1)
    * ```
    */
  def ifExpr: Parser[untpd.IfExpr] = {
    ("if" ~ exprParser ~ "then" ~ exprParser ~ (NL.optional >> "else") ~ exprParser) <| {
      case _ ~ cond ~ _ ~ trueBody ~ _ ~ falseBody =>
        Untyped(Trees.IfExpr(cond, trueBody, falseBody))
    }
  }
  
  def newExpr: Parser[untpd.NewExpr] = {
    val params: Parser[List[untpd.Expr]] = exprParser.sepBy(",").wrappedBy("(", ")").optional <| {
      case None => Nil
      case Some(xs) => xs
    }
    ("new" ~ identifier ~ params) <| { case _ ~ ScalaToken(ScalaTokenType.Identifier(symName)) ~ params =>
      Untyped(Trees.NewExpr(Symbol.Ref.Unresolved(symName), params))
    }
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
      { "." ~ identifier } <| { case _ ~ ScalaToken(ScalaTokenType.Identifier(member)) =>
        e => Untyped(Trees.SelectExpr(e, member))
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
  def exprTerm: Parser[untpd.Expr] = identifierExpr or literals or { lambdaExpr or blockExpr or exprParser.wrappedBy("(", ")") }

  /** Parses literal values.
    */
  def literals: Parser[untpd.Expr] = literalStringExpr or literalIntExpr or literalFloatExpr or literalBooleanExpr

  /** Parses a integer literal.
    */
  def literalIntExpr: Parser[untpd.Expr] = literalInt <| {
    case ScalaToken(ScalaTokenType.LiteralInt(value)) => Untyped(Trees.LiteralIntExpr(value))
  }

  /** Parses a floating number literal.
    */
  def literalFloatExpr: Parser[untpd.Expr] = literalFloat <| {
    case ScalaToken(ScalaTokenType.LiteralFloat(value)) => Untyped(Trees.LiteralFloatExpr(value))
  }

  /** Parses a boolean literal.
    */
  def literalBooleanExpr: Parser[untpd.Expr] = literalBoolean <| {
    case ScalaToken(ScalaTokenType.LiteralBoolean(value)) => Untyped(Trees.LiteralBooleanExpr(value))
  }

  def literalStringExpr: Parser[untpd.Expr] = literalString <| {
    case ScalaToken(ScalaTokenType.LiteralString(value)) => Untyped(Trees.LiteralStringExpr(value))
  }

  /** Parser for lambda expressions.
    */
  def lambdaExpr: Parser[untpd.Expr] = {
    val param: Parser[(ScalaToken, Type)] = (identifier ~ ":" ~ typeParser) <| { case n ~ _ ~ t => (n, t) }
    val params: Parser[List[Symbol[Trees.LambdaParam]]] = param.sepBy(",").wrappedBy("(", ")") <| { ps =>
      // create a new scope for the lambda
      scopeCtx.locateScope()
      def recur(ps: List[(ScalaToken, Type)]): List[Symbol[Trees.LambdaParam]] = ps match {
        case Nil => Nil
        case (tk @ ScalaToken(ScalaTokenType.Identifier(name)), tpe) :: ps =>
          if scopeCtx.findSymHere(name).isDefined then
            throw SyntaxError(s"duplicated parameter name in lambda definition: $name").withPos(tk.pos)
          else {
            val lambdaParam: Trees.LambdaParam = Trees.LambdaParam(Symbol(name, null), tpe)
            lambdaParam.sym.dealias = lambdaParam
            scopeCtx.addSymbol(lambdaParam.sym)
            lambdaParam.sym :: recur(ps)
          }
        case _ :: ps => recur(ps)
      }
      recur(ps)
    }
    val body: Parser[untpd.Expr] = exprParser <| { x =>
      // exit the scope
      scopeCtx.relocateScope()
      Untyped(x.tree)
    }

    (params ~ "=>" ~ body) <| { case params ~ _ ~ body => Untyped(Trees.LambdaExpr(params, None, body)) }
  }

  /** Parser for block expressions.
    */
  def blockExpr: Parser[untpd.BlockExpr] = {
    val line: Parser[untpd.LocalDef] = localDef << NL
    val begin: Parser[ScalaToken] = "{" <| { t => scopeCtx.locateScope(); t }
    val end: Parser[ScalaToken] = "}" <| { t => scopeCtx.relocateScope(); t }
    val block: Parser[untpd.BlockExpr] = 
      (begin ~ line.many ~ end) <| { case beginToken ~ ls ~ endToken => 
        ls match {
          case Nil => throw SyntaxError(s"Block expression should not be empty").withPos(endToken.pos)
          case ls : List[untpd.LocalDef] =>
            val lastOne: Trees.LocalDef[Untyped] = ls.last.tree
            lastOne match {
              case eval: fs2c.ast.fs.Trees.LocalDef.Eval[Untyped] =>
                Untyped(Trees.BlockExpr(ls.init, eval.expr))
              case _ =>
                throw SyntaxError(s"Expecting a expression at the end of a block.").withPos(endToken.pos)
            }
        }
      }

    block
  }

  def localDef: Parser[untpd.LocalDef] = localDefWhile | localDefBind | localDefAssign | localDefEval | localDefDef

  def localDefDef: Parser[untpd.LocalDef] = {
    val param: Parser[(ScalaToken, Type)] = (identifier ~ ":" ~ typeParser) <| { case n ~ _ ~ t => (n, t) }
    ("def" ~ identifier ~ param.sepBy(",").wrappedBy("(", ")") ~ "=" ~ exprParser) <| { case _ ~ name ~ params ~ _ ~ body =>
      val funcName = name match {
        case ScalaToken(ScalaTokenType.Identifier(name)) => name
      }
      scopeCtx.locateScope()
      val funcParams = params map {
        case (tk @ ScalaToken(ScalaTokenType.Identifier(name)), tp) =>
          if scopeCtx.findSymHere(name).isDefined then
            throw SyntaxError(s"duplicated parameter name in lambda definition: $name").withPos(tk.pos)
          else {
            val lambdaParam: Trees.LambdaParam = Trees.LambdaParam(Symbol(name, null), tp)
            lambdaParam.sym.dealias = lambdaParam
            scopeCtx.addSymbol(lambdaParam.sym)
            lambdaParam.sym
          }
      }
      val lambda = Untyped(Trees.LambdaExpr(funcParams, None, body))
      if scopeCtx.findSym(funcName).isDefined then
        throw SyntaxError(s"duplicated value name in local definition: $funcName").withPos(name.pos)
      else {
        val bind: untpd.LocalDefBind = Untyped(Trees.LocalDef.Bind(Symbol(funcName, null), false, None, lambda))
        bind.tree.sym.dealias = bind
        scopeCtx.addSymbol(bind.tree.sym)
        bind
      }
    }
  }

  def localDefBind: Parser[untpd.LocalDef] = {
    val kw: Parser[ScalaTokenType] = ("val" | "var") <| { case ScalaToken(t) => t }
    val ascription: Parser[Option[Type]] = (":" >> typeParser).optional
    (kw ~ identifier ~ ascription ~ "=" ~ exprParser) <| { 
      case bindKw ~ (t @ ScalaToken(ScalaTokenType.Identifier(name))) ~ tpe ~ _ ~ body =>
        val mutable = bindKw match {
          case ScalaTokenType.KeywordVal => false
          case ScalaTokenType.KeywordVar => true
          case _ => false
        }

        if scopeCtx.findSymHere(name).isDefined then
          throw SyntaxError(s"duplicated value name in local definition: $name").withPos(t.pos)
        else {
          val bind: untpd.LocalDefBind = Untyped(Trees.LocalDef.Bind(Symbol(name, null), mutable, tpe, body))
          bind.tree.sym.dealias = bind
          scopeCtx.addSymbol(bind.tree.sym)
          bind
        }
    }
  }
  
  def localDefAssign: Parser[untpd.LocalDef] = {
    (identifier ~ "=" ~ exprParser) <| { case t@ScalaToken(ScalaTokenType.Identifier(symName)) ~ _ ~ expr =>
      Untyped(Trees.LocalDef.Assign(tryResolveSymbol(symName), expr))
    }
  }

  def localDefEval: Parser[untpd.LocalDef] = exprParser <| { (expr: untpd.Expr) =>
    Untyped(Trees.LocalDef.Eval(expr))
  }

  def localDefWhile: Parser[untpd.LocalDef] = ("while" ~ exprParser ~ "do" ~ exprParser) <| { case _ ~ cond ~ _ ~ body =>
    Untyped(Trees.LocalDef.While(cond, body))
  }
  
  def tryResolveSymbol(symName: String): Symbol.Ref = scopeCtx.findSym(symName) match {
    case None => Symbol.Ref.Unresolved(symName)
    case Some(sym) => Symbol.Ref.Resolved(sym)
  }

  /** Parser for identifiers in the expression.
    */
  def identifierExpr: Parser[untpd.IdentifierExpr | untpd.GroundValue] = {
    identifier <| {
      case ScalaToken(ScalaTokenType.Identifier(symName)) =>
        groundValueMap get symName match {
          case Some(e) => e
          case None =>
            scopeCtx.findSym(symName) match {
              case None => Untyped(Trees.IdentifierExpr[Untyped](Symbol.Ref.Unresolved(symName)))
              case Some(sym) => Untyped(Trees.IdentifierExpr[Untyped](Symbol.Ref.Resolved(sym)))
            }
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
  
  def typeTerm: Parser[Type] = groundType or { typeParser.wrappedBy("(", ")") } or symbolType
  
  def groundType: Parser[GroundType] = (symbol("Int") <* GroundType.IntType) or
      (symbol("Float") <* GroundType.FloatType) or
      (symbol("Boolean") <* GroundType.BooleanType) or
      (symbol("String") <* GroundType.StringType) or
      arrayType
  
  def arrayType: Parser[GroundType.ArrayType] =
    (symbol("Array") seq "[" seq typeParser seq "]") <| {
      case _ ~ _ ~ tpe ~ _ => GroundType.ArrayType(tpe)
    }
  
  def symbolType: Parser[Types.SymbolType] = identifier <| {
    case ScalaToken(ScalaTokenType.Identifier(symName)) =>
      SymbolType(tryResolveSymbol(symName))
  }
}

object ScalaParser {
  def parseSource[X](p: Parser[X], source: ScalaSource): Result[X] = {
    val tokenizer = new Tokenizer(source)
    parseWithTokenizer(p, tokenizer)
  }
  
  /** --- debug --- */
  def parseString[X](p: Parser[X], str: String): Result[X] = {
    val tokenizer = new Tokenizer(ScalaSource.testSource(str))
    parseWithTokenizer(p, tokenizer)
  }
  
}

package fs2c

import fs2c.io.ScalaSource
import fs2c.ast.fs.Trees.{tpd, untpd}
import fs2c.typer.Types
import fs2c.ast.c.{Trees => C}
import fs2c.codegen.{CodeGen, CodePrinter, CodeBundles => bd}
import fs2c.tools.packratc.scala_token.ScalaTokenParser
import fs2c.tools.packratc.scala_token.ScalaTokenParser.latest
import parser.ScalaParser
import typer.Typer

class Compiler {
  import Compiler.ParseError

  protected var typer = new Typer
  

  /** Creates a [[ScalaSource]] instance from given file path.
    */
  def loadFile(path: String): ScalaSource =
    ScalaSource(path)

  /** Parses a file.
    */
  def parseFile(path: String): List[untpd.ClassDef] = {
    val source = loadFile(path)
    val parser = (new ScalaParser).fileParser
    val ctx = new ScalaTokenParser.ParserContext(Nil)
    ScalaParser.runParserWithSource(parser, source)(using ctx) match {
      case Left(v) =>
        val err = v.latest(ctx.generatedErrors)
        throw ParseError(err)
      case Right(v) => v._1
    }
  }
  
  def typedClassDefs(clsDefs: List[untpd.ClassDef]): List[tpd.ClassDef] =
    clsDefs map typer.typedClassDef
  
  def typedFile(path: String): List[tpd.ClassDef] =
    typedClassDefs(parseFile(path))

  def genClassDefs(defs: List[tpd.ClassDef]): (List[C.Definition], List[String]) = {
    val foundMain = defs.find { d =>
      val clsDef = d.tree
      clsDef.sym.name == "Main" && { clsDef.params.isEmpty } && {
        clsDef.members exists { m =>
          m.tree.sym.name == "main" && {
            m.tree.body.tpe match {
              case Types.LambdaType(Nil, _) => true
              case _ => false
            }
          }
        }
      }
    }

    foundMain match {
      case None =>
        throw Compiler.CompileError(s"can not extract Main.main method")
      case Some(mainClass) =>
        val codegen = new CodeGen
        defs foreach { d => codegen.genClassDef(d) }
        mainClass.code match {
          case bundle: bd.ClassBundle =>
            val structDef = bundle.structDef
            codegen.makeMainFunc(structDef, bundle.initDef, {
              val mainFunc = mainClass.tree.members find { m => m.tree.sym.name == "main" }
              mainFunc.get.tpe match {
                case t: Types.LambdaType => t
                case _ => assert(false, "main function should be a lambda")
              }
            })
            (codegen.ctx.generatedDefs.reverse, codegen.ctx.included)
          case _ =>
            assert(false, "code not generated for main class")
        }
    }
  }

  def outputCode(ds: List[C.Definition], included: List[String]): String = {
    val printer = new CodePrinter(ds, included)
    printer.sourceContent
  }
}

object Compiler {
  case class ParseError(err: ScalaTokenParser.ParseError) extends Exception {
    override def toString: String = err.toString
  }
  case class CompileError(errMsg: String) extends Exception(errMsg)
}

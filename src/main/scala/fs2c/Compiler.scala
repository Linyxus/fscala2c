package fs2c

import fs2c.io.ScalaSource
import fs2c.ast.fs.Trees.{tpd, untpd}

import parser.ScalaParser
import typer.Typer

class Compiler {
  import Compiler.ParseError

  protected var typer = new Typer
  

  /** Creates a [[ScalaSource]] instance from given file path.
    */
  def loadFile(path: String): ScalaSource =
    ScalaSource.fromPath(path)

  /** Parses a file.
    */
  def parseFile(path: String): List[untpd.ClassDef] = {
    val source = loadFile(path)
    ScalaParser.parseSource((new ScalaParser).fileParser, source) match {
      case Left(v) =>
        throw ParseError(v.toString)
      case Right(v) => v._1
    }
  }
  
  def typedClassDefs(clsDefs: List[untpd.ClassDef]): List[tpd.ClassDef] =
    clsDefs map typer.typedClassDef
  
  def typedFile(path: String): List[tpd.ClassDef] =
    typedClassDefs(parseFile(path))
}

object Compiler {
  case class ParseError(errMsg: String) extends Exception(errMsg)
}

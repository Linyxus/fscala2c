package fs2c.codegen

import scala.language.implicitConversions
import fs2c.printing.Printer.{*, given}
import fs2c.printing.Printer
import fs2c.printing.printing.c
import c.{*, given}
import fs2c.ast.c.{Trees => C}

class CodePrinter(defs: List[C.Definition], included: List[String]) {
  private var outputed: StringBuilder = new StringBuilder

  def showDefinitionDecl(d: C.Definition): Option[String] = d match {
    case C.FuncDef(sym, retType, params, _) =>
      val printer = new Printer
      given Printer = printer
      c.cType.print(retType)
      printer.print(s" ${sym.name}")
      printer.inParen {
        printer.printSepBy(params) { p =>
          c.cType.print(p.tp)
          printer.print(" ")
          printer.print(p.sym.name)
        }
      }
      printer.print(";\n")
      Some(printer.result)
    case C.StructDef(sym, _) =>
      Some(s"struct ${sym.name};\n")
    case _: C.TypeAliasDef =>
      None
  }

  def outputLine(line: String): Unit = {
    outputed ++= line
    outputed ++= "\n"
  }

  def includeFile(file: String): Unit = outputLine(s"#include <$file>\n")

  def declareDefinition(d: C.Definition): Unit = showDefinitionDecl(d) foreach { s => outputLine(s) }

  def defineDefinition(d: C.Definition): Unit = outputLine(d.show)

  private def content(): String = outputed.result()

  lazy val sourceContent: String = {
    included foreach includeFile
    defs foreach declareDefinition
    defs foreach defineDefinition
    content()
  }
}

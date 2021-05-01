package fs2c

import scopt.OParser

case class CompilerConfig(sourcePath: String = "<empty>")

object Main {
  def main(args: Array[String]): Unit = {
    val builder = OParser.builder[CompilerConfig]
    val parser1 = {
      import builder._
      OParser.sequence(
        programName("fscala2c"),
        head("fscala2c", "0.1.0"),
        opt[String]('s', "source")
          .required()
          .action((x, c) => c.copy(sourcePath = x))
          .text("source file path")
      )
    }

    println(args.toList)

    OParser.parse(parser1, args, CompilerConfig()) match {
      case Some(CompilerConfig(sourcePath)) =>
        val compiler = new Compiler
        val defs = compiler.typedFile(sourcePath)
        val (cDefs, includes) = compiler.genClassDefs(defs)
        val cSource = compiler.outputCode(cDefs, includes)
        println(cSource)
      case _ =>
    }
  }
}

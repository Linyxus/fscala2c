package fs2c

import scopt.OParser
import java.io.{File, FileWriter, BufferedWriter}

case class CompilerConfig(sourcePath: String = "<empty>", outputPath: Option[String] = None)

object Main {
  def outputFile(path: String, content: String): Unit = {
    val f = new File(path)
    val bw = new BufferedWriter(new FileWriter(f))
    bw.write(content)
    bw.close()
  }

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
          .text("source file path"),
        opt[String]('o', "output")
          .action((x, c) => c.copy(outputPath = Some(x)))
          .text("output file path")
      )
    }

    OParser.parse(parser1, args, CompilerConfig()) match {
      case Some(CompilerConfig(sourcePath, outputPath)) =>
        val compiler = new Compiler
        val defs = compiler.typedFile(sourcePath)
        val (cDefs, includes) = compiler.genClassDefs(defs)
        val cSource = compiler.outputCode(cDefs, includes)
        println(cSource)

        outputPath foreach { outputPath =>
          println("writing to output file ...")
          outputFile(outputPath, cSource)
        }
      case _ =>
    }
  }
}

package fs2c

import org.junit.Assert._
import org.junit.Test

class TestCompiler {
  
  @Test def typer: Unit = {
    val sources = List(
      "tests/point.scala",
      "tests/even.scala",
      "tests/fact.scala",
      "tests/bigrec.scala",
    )
    val compiler = new Compiler
    sources foreach { s => compiler.typedFile(s) }
  }

  @Test def codegen: Unit = {
    val compiler = new Compiler
    val ds = compiler.typedFile("tests/point.scala")
    val (cds, inc) = compiler.genClassDefs(ds)
    compiler.outputCode(cds, inc)
  }

}

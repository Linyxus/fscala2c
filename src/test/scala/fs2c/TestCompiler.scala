package fs2c

import org.junit.Assert._
import org.junit.Test

class TestCompiler {
  
  @Test def typer: Unit = {
    val sources = List(
      "tests/point.scala",
      "tests/even.scala",
      "tests/fact.scala",
    )
    val compiler = new Compiler
    sources foreach { s => compiler.typedFile(s) }
  }

}

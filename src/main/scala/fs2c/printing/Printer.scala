package fs2c.printing

import scala.language.implicitConversions

class Printer(val tabSize: Int = 2) {
  /** Current indent level.
    */
  private var indentLevel: Int = 0
  
  /** Has outputed tab of the line.
    */
  private var outputedTab: Boolean = false

  /** Outputed string.
    */
  private var outputed: StringBuilder = new StringBuilder

  /** Output indenting tab.
    */
  def maybeTab(): Unit =
    !outputedTab && {
      outputedTab = true
      outputed ++= " ".repeat(indentLevel)
      true
    }

  /** Start a new line.
    */
  def newLine(): Unit = {
    outputedTab = false
    outputed ++= "\n"
  }

  /** Start a new indenting block.
    */
  def indenting[T](body: => T): T = {
    val origIndentLevel = indentLevel
    
    indentLevel += tabSize
    val res = body
    indentLevel = origIndentLevel
    
    res
  }

  /** Print a string.
    */
  def print(str: String): Unit = {
    maybeTab()
    outputed ++= str
  }

  /** Print a string and start a new line.
    */
  def println(str: String): Unit = {
    print(str)
    newLine()
  }

  /** Output text blocks in braces.
    */
  def inBlock[T](body: => T): T = {
    print("{")
    var res = indenting { body }
    println("}")
    
    res
  }
  
  def result: String = outputed.result()
}

object Printer {
  trait Showable {
    def show: String
  }
  
  trait Printing[T] {
    def print(t: T)(using Printer): Unit
  }
  
  given toShowable[T](using printing: Printing[T]): Conversion[T, Showable] with
    def apply(t: T): Showable = new Showable {
      def show = {
        given printer: Printer = new Printer
        printing.print(t)
        printer.result
      }
    }
}
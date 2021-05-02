package fs2c.io

case class SourcePos(source: ScalaSource, line: Int, col: Int, idx: Int) {
  def lineStr: String = source.lines(line)
}

case class SourcePosSpan(start: SourcePos, length: Int) {
  lazy val showInSourceLine: String = {
    val lineNum = start.line
    val linePos = start.col
    val lineStr = start.lineStr

    val header = s" ${lineNum + 1} | "
    val signSpace = " ".repeat(header.length + linePos)
    val sign = "^".repeat(if length <= 0 then 1 else length)
    s"$header$lineStr\n$signSpace$sign"
  }
}

trait Positional {
  type PosSelf >: this.type

  private var myPos: SourcePosSpan = null

  def pos: SourcePosSpan = myPos

  def withPos(newPos: => SourcePosSpan): PosSelf = {
    myPos = newPos
    this
  }

  /** Show the positional element in the line.
    *
    * @return
    */
  def showInSourceLine: String = myPos match {
    case null => "<no position>"
    case pos => pos.showInSourceLine
  }
}

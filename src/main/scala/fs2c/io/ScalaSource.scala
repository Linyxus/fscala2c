package fs2c.io

import io.Source

/** Abstract interface for Scala source file.
  */
abstract class ScalaSource {
  /** Source content.
    * @return
    */
  def content: String

  /** Lines in source content.
    *
    * @return
    */
  def lines: Array[String]

  /** Number of lines in the source.
    *
    * @return
    */
  def lineCount: Int = lines.length

  lazy val lineLengths: Array[Int] = lines map { line => line.length }

  /** Locate source position with the given index.
    *
    * @param posIdx
    * @return
    */
  def locatePos(posIdx: Int): SourcePos = {
    var currentLine = 0
    var currentIdx = 0
    var col = posIdx
    var n = 0

    while currentIdx <= posIdx && currentLine < lineCount do {
      n = lineLengths(currentLine) + 1
      currentIdx += n
      col -= n
      currentLine += 1
    }

    currentLine -= 1
    col += n

    if currentLine >= lineCount then
      col = 0

    SourcePos(this, currentLine, col)
  }
}

case class TestSource(fileName: String, myContent: String) extends ScalaSource {
  override def content = myContent
  override lazy val lines = myContent.split('\n').toArray
}

class FileSource(val path: String) extends ScalaSource {
  override val lines: Array[String] = {
    val src = Source.fromFile(path)
    val res = src.getLines().toArray
    src.close()
    res
  }

  override val content: String = lines mkString "\n"
}

object ScalaSource {
  def apply(path: String): ScalaSource = new FileSource(path)

  def testSource(content: String): ScalaSource = new TestSource("test", content)
}

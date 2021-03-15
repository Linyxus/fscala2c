package fs2c.io

import io.Source

case class ScalaSource(path: String, content: String)

object ScalaSource {
  def fromPath(path: String): ScalaSource = {
    val f = Source.fromFile(path)
    val src = ScalaSource(path, f.getLines() mkString "\n")
    f.close()
    src
  }
}

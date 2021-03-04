package fs2c.io

case class SourcePos(source: ScalaSource, pos: Int) {
  def extractLine: (Int, Int, String) = {
    if pos > source.content.length then
      (-1, -1, "")
    else {
      var lineStart = 0
      var current = 0
      var lineNum = 0
      var linePos = 0
      while (current < pos && current < source.content.length) do {
        if source.content(current) == '\n' then {
          lineStart = current + 1
          linePos = -1
          lineNum += 1
        }
        current += 1
        linePos += 1
      }
      while current < source.content.length && source.content(current) != '\n' do
        current += 1
      val lineEnd = current
      if linePos == -1 then linePos = 0
      (lineNum, linePos, source.content.substring(lineStart, lineEnd))
    }
  }
}

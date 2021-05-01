class Main {
  val main = () => {
    val n = readInt()
    var s = 0
    var i = 0
    while i <= n do {
      s = s + i
      i = i + 1
      i
    }
    printInt(s)
  }
}
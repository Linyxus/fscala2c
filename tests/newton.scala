class Main {
  val abs = (i: Float) => if i < 0.0 then 0.0 - i else i

  val squareRoot = (n: Float) => {
    val loss = 0.00001
    var root = n
    var prev = n + 100.0

    while (abs(root - prev) > loss) do {
      prev = root
      root = 0.5 * (root + (n / root))
      root
    }

    root
  }

  val main = () => {
    val n = readFloat()
    printFloat(n)
    val sq = squareRoot(n)

    printFloat(sq)
  }
}
class Main {
  val sum = (n: Int, callback: Int => Int) => {
    val res =
      if n <= 0 then
        0
      else
        sum(n - 1, (t: Int) => n + t)
    callback(res)
  }

  val main = () => {
    val n = readInt()
    printlnInt(n)
    val s = sum(n, (x: Int) => x)
    printlnInt(s)
  }
}
class Main {
  val sum: (Int, Int => Int) => Int = (n: Int, callback: Int => Int) => {
    if n <= 0 then
      callback(0)
    else
      sum(n - 1, (t: Int) => callback(n + t))
  }

  val main = () => {
    val n = readInt()
    printlnInt(n)
    val s = sum(n, (x: Int) => x)
    printlnInt(s)
  }
}
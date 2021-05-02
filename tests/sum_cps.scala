class Main {
  val sum: (Int, Int => Int) => Int = (n: Int, callback: Int => Int) => {
    if n <= 0 then
      callback(0)
    else
      sum(n - 1, (t: Int) => callback(n + t))
  }

  val main = () => {
    print("> ")
    val n = readInt()
    val s = sum(n, (x: Int) => x)
    print("sum(")
    printInt(n)
    print(") = ")
    printlnInt(s)
  }
}

class Main {
  val fibonacci: (Int, Int => Int) => Int = (n: Int, callback: Int => Int) =>
    if n <= 1 then
      callback(1)
    else
      fibonacci(n - 1, (t1: Int) => fibonacci(n - 2, (t2: Int) => callback(t1 + t2)))

  val identity = (x: Int) => x

  val main = () => {
    val n = readInt()
    val res = fibonacci(n, identity)
    printlnInt(res)
  }
}

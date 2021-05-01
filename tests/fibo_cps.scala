
class Main {
  val fibonacci = (n: Int, callback: Int => Int) =>
    if n <= 1 then
      callback(1)
    else {
      val res = fibonacci(n - 1, (t1: Int) => fibonacci(n - 2, (t2: Int) => t1 + t2))
      callback(res)
    }

  val identity = (x: Int) => x

  val main = () => {
    val n = readInt()
    val res = fibonacci(n, identity)
    printlnInt(res)
  }
}

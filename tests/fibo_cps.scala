
class Main {
  def fibonacci(n: Int, callback: Int => Int) =
    if n <= 1 then
      callback(1)
    else
      fibonacci(n - 1, (t1: Int) => fibonacci(n - 2, (t2: Int) => callback(t1 + t2)))

  def main = {
    val n = readInt()
    var i = 0
    while i <= n do {
      val f = fibonacci(i, (x: Int) => x)
      printf("fibonacci(%d) = %d\n", i, f)
      i = i + 1
    }
  }
}

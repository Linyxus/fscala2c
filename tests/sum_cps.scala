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

    var i = 0
    while i <= n do {
      val s = sum(i, (x: Int) => x)
      printf("sum(0..%d) = %d\n", i, s)
      i = i + 1
    }
  }
}
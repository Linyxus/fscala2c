class Main {
  val fibonacci: Int => Int = (n: Int) =>
    if n <= 1 then
      1
    else
      fibonacci(n - 1) + fibonacci(n - 2)

  val main = () => {
    val n = readInt()
    val f = fibonacci(n)
    printlnInt(f)
  }
}
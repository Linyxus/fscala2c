class Main {
  def main = {
    /* return a function that applies `f` twice on input */
    def twice(f: Int => Int) =
      (x: Int) => f(f(x))

    val add4 = twice(twice((x: Int) => x + 1))

    printlnInt(add4(2))
  }
}
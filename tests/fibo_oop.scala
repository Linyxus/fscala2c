class FiboSolver {
  var cache = {
    val res = Array[Int](10000)
    res(0) = 1
    res(1) = 1
    res
  }

  var current = 1

  def step = {
    current = current + 1
    cache(current) = cache(current - 1) + cache(current - 2)
  }

  def stepTo(i: Int) = {
    while current < i do step()
  }

  def solve(i: Int): Int = {
    stepTo(i)
    cache(i)
  }
}

class Main {
  def read: Int = {
    print("> ")
    readInt()
  }

  def main = {
    val solver = new FiboSolver()
    while true do {
      val i = read()
      val res = solver.solve(i)
      printf("fibo(%d) = %d\n", i, res)
    }
  }
}
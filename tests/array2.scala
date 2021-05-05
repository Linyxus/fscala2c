class Main {
  def main = {
    val xs = Array[Int](10)
    var i = 0
    while i < 10 do {
      xs(i) = i * i
      i = i + 1
    }
    i = 0
    while i < 10 do {
      printf("xs[%d] = %d\n", i, xs(i))
      i = i + 1
    }
  }
}
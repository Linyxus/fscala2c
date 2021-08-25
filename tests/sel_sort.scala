class Main {
  def selectionSort(arr: Array[Int], n: Int) = {
    var i = 0
    while (i < n - 1) do {
      var min = i
      var j = i + 1
      while j < n do {
        if (arr(j) < arr(min)) then {
          min = j
          j = j + 1
        } else {
          j = j + 1
        }
      }
      val t = arr(i)
      arr(i) = arr(min)
      arr(min) = t
      i = i + 1
    }
  }

  def main = {
    var l = 0
    var xs = Array[Int](100)
    var x = readInt()
    while x != -1 do {
      xs(l) = x
      x = readInt()
      l = l + 1
    }
    selectionSort(xs, l)

    var i = 0
    while i < l do {
      printf("%d ", xs(i))
      i = i + 1
    }
    println("")
  }
}

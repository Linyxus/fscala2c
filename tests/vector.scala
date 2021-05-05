class Vector(n: Int, fill: Int) {
  var mySize: Int = n
  var myCapacity: Int = n

  def size = mySize

  def capacity = myCapacity

  def enlarge: Int = {
    val newCapacity = myCapacity + 10
    printf("vector is enlarged! %d ->> %d\n", myCapacity, newCapacity)
    val newData: Array[Int] = Array[Int](newCapacity)
    var i = 0
    while i < mySize do {
      newData(i) = data(i)
      i = i + 1
    }
    data = newData
    myCapacity = newCapacity
    myCapacity
  }

  var data: Array[Int] = {
    val res = Array[Int](n)
    var i = 0
    while i < n do {
      res(i) = fill
    }
    res
  }

  def get(idx: Int) = data(idx)

  def set(idx: Int, v: Int): Unit = {
    data(idx) = v
  }

  def pushBack(v: Int) = {
    mySize = mySize + 1
    if mySize > myCapacity then enlarge() else 0
    data(mySize - 1) = v
    printf("after push back: %s\n", show())
  }

  def show: String = format("Vector(size=%d, capacity=%d)", mySize, myCapacity)

  def isEmpty: Boolean = mySize == 0
}

class Main {
  def read: Int = {
    print("input int, -1 to terminate >> ")
    readInt()
  }

  def printVector(xs: Vector) =
    if xs.isEmpty() then
      print("[]")
    else {
      print("[")
      printInt(xs.get(0))
      var i = 1
      while i < xs.size() do {
        printf(", %d", xs.get(i))
        i = i + 1
      }
      print("]")
    }

  def main = {
    val arr = new Vector(0, 0)
    var i = read()
    while i != -1 do {
      arr.pushBack(i)
      print("the vector is ")
      printVector(arr)
      println("")
      i = read()
    }
  }
}
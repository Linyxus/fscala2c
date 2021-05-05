class IntWrapper(init: Int) {
  var value = init

  def isZero: Boolean = value <= 0

  def show: String = format("IntWrapper(%d)", value)
}

class Main {
  def inc(i: IntWrapper) = {
    i.value = i.value + 1
  }

  def dec(i: IntWrapper) = {
    i.value = i.value - 1
  }

  def read: IntWrapper = {
    print("> ")
    new IntWrapper(readInt())
  }

  def plus(i: IntWrapper, j: IntWrapper): IntWrapper =
    if j.isZero() then
      i
    else {
      inc(i)
      dec(j)
      plus(i, j)
    }

  def main = {
    val i1 = read()
    val i2 = read()
    val s1 = i1.show()
    val s2 = i2.show()
    val i3 = plus(i1, i2)
    val s3 = i3.show()
    printf("%s + %s = %s\n", s1, s2, s3)
  }
}
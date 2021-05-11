class Main {
  def gcd(a: Int, b: Int): Int =
    if b == 0 then a
    else gcd(b, a % b)

  def main = {
    val a = readInt()
    val b = readInt()
    val res = gcd(a, b)
    printf("gcd(%d, %d) = %d\n", a, b, res)
  }
}
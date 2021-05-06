class Main {
  def even(n: Int) =
    if n <= 0 then true
    else !odd(n - 1)

  def odd(n: Int) =
    if n <= 0 then false
    else -even(n - 1)

  def main = {
    0
  }
}
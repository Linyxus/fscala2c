class Fact {
  val fact = (n : Int) =>
    if n == 0 then
      1
    else
      n * fact(n - 1)
}
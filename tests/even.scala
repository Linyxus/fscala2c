class Main {
  val odd = (n : Int) => if n == 0 then false else !even(n - 1)
  val even = (n : Int) => if n == 0 then true else !odd(n - 1)
  
  val x = 10
  val xIsOdd = odd(x)
  val xIsEven = even(x)
}
class Main {
  val input = () => {
    print("> ")
    val i = readInt()
    i
  }

  val main = () => {
    val a = input()
    val b = input()
    val r = a % b
    printlnInt(r)
  }
}
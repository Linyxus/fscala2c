class Main {
  def makeGreeter(name: String) = () => println(greeterStr(name))

  def greeterStr(name: String) = format("Hello, %s", name)

  def main = {
    val greeter = makeGreeter("FScala")
    greeter()
  }
}
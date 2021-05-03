class Point(x0: Float, y0: Float) {
  val x = x0
  val y = y0

  val distTo = (other: Point) => {
    val dx = x - other.x
    val dy = y - other.y
    sqrt(dx * dx + dy * dy)
  }
}

class Main {
  val readPoint = () => {
    print("input x: ")
    val x = readFloat()
    print("input y: ")
    val y = readFloat()
    new Point(x, y)
  }

  val printPoint = (p: Point) => printf("(%f, %f)", p.x, p.y)

  val main = () => {
    println("input the first point!")
    val p1 = readPoint()
    println("input the second point!")
    val p2 = readPoint()
    print("the two points are ")
    printPoint(p1)
    print(" and ")
    printPoint(p2)
    println(".")
    val d = p1.distTo(p2)
    printf("the distance between them: %f\n", d)
  }
}
class Point(x0 : Int, y0 : Int) {
  val x = x0
  val y = y0
  
  val abs = (i : Int) => if i < 0 then 0 - i else i
  
  val distTo = (other : Point) => {
    val dx = abs(x - other.x)
    val dy = abs(y - other.y)
    dx + dy
  }
  
  val distToOrigin = () => distTo(new Point(0, 0))
}

class Main {
  val p1 = new Point(1, 1)
  val p2 = new Point(3, 4)
  
  val d1 = p1.distTo(p2)
  val d2 = p1.distToOrigin()

  val main = () => {
    d1 + d2
  }
}

package fs2c.tools.data

trait LazyListTools {
  def fromList[X](xs: List[X]): LazyList[X] = {
    @annotation.tailrec def recur(xs: List[X], acc: LazyList[X]): LazyList[X] = xs match {
      case Nil => acc
      case x :: xs => recur(xs, x #:: acc)
    }
    
    recur(xs.reverse, LazyList())
  }
}

object LazyListTools extends LazyListTools

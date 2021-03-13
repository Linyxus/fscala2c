package fs2c.tools

import collection.mutable

object Unique {
  val nameCount = mutable.Map.empty[String, Int]
  
  def uniqueName(prefix: String): String = {
    val cnt = nameCount.getOrElse(prefix, 0)
    val res = prefix + "$" + cnt.toString
    nameCount.update(prefix, cnt + 1)
    res
  }
}

package fs2c.tools

import fs2c.typer.Types.TypeVariable
import collection.mutable

object Unique {
  val nameCount = mutable.Map.empty[String, Int]

  def reset(): Unit = nameCount.clear()

  def uniqueName(prefix: String): String = {
    val cnt = nameCount.getOrElse(prefix, 0)
    val res = prefix + "$" + cnt.toString
    nameCount.update(prefix, cnt + 1)
    res
  }
  
  def freshTypeVar(prefix: String = "X"): TypeVariable =
    TypeVariable(uniqueName(prefix), Nil)
    
  var globalNameCount: Int = 0
  
  def nextHexPostfix(): String = {
    val s = globalNameCount.toHexString
    globalNameCount += 1
    s
  }
  
  def uniqueCName(name: String): String = name + "__" + nextHexPostfix()
}

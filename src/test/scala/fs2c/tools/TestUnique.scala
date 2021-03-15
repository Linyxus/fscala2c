package fs2c.tools

import org.junit.Assert._
import org.junit.Test

import fs2c.tools.Unique

class TestUnique {
  
  @Test def uniqueNames: Unit = {
    import Unique._
    reset()
    
    assertEquals(uniqueName("X"), "X$0")
    assertEquals(uniqueName("X"), "X$1")
    assertEquals(uniqueName("X"), "X$2")
    assertEquals(uniqueName("T"), "T$0")
    assertEquals(uniqueName("X"), "X$3")
    assertEquals(uniqueName("T"), "T$1")
  }
  
}

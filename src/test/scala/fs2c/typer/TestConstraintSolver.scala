package fs2c.typer

import Types._
import GroundType._
import Constraints._
import fs2c.tools.Unique

import org.junit.Assert._
import org.junit.Test

class TestConstraintSolver {
  @Test def simple: Unit = {
    val solver = new ConstraintSolver
    
    val tv1 = Unique.freshTypeVar()
    val tv2 = Unique.freshTypeVar()
    
    solver.addEquality(LambdaType(List(tv1), ArrayType(tv1)), LambdaType(List(ArrayType(StringType)), tv2))
    val subst = solver.subst
    assertEquals(ArrayType(StringType), subst(tv1))
    assertEquals(ArrayType(ArrayType(StringType)), subst(tv2))
  }
}

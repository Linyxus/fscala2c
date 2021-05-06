package fs2c.typer

import scala.io.AnsiColor._
import Types._
import fs2c.typer
import fs2c.ast.fs.Trees
import Typer.TypeError
import fs2c.io.{Positional, SourcePosSpan}

object Constraints {

  /** Enum for constraints.
    */
  enum Constraint extends Positional {
    case Equality(tpe1: Type, tpe2: Type, lhs: Option[SourcePosSpan] = None, rhs: Option[SourcePosSpan] = None)

    override def toString: String = this match {
      case Equality(tp1, tp2, lhs, rhs) =>
        val desc = s"$tp1 == $tp2"
        val lhsDesc = lhs.map(lhs => s"$BOLD$RED, where the LHS of the equality ($tp1)$RESET\n" ++ lhs.showInSourceLine(hint = s"is from here")).getOrElse("")
        val rhsDesc = rhs.map(rhs => s"$BOLD$RED, where the RHS of the equality ($tp2)$RESET\n" ++ rhs.showInSourceLine(hint = s"is from here")).getOrElse("")
        desc ++ lhsDesc ++ rhsDesc
    }

    def deriveEquality(tp1: Type, tp2: Type): Equality = this match {
      case Equality(_, _, lhs, rhs) => Equality(tp1, tp2, lhs, rhs).withPos(this).asInstanceOf[Equality]
    }
  }
  
  import Constraint._

  /** Substitions are mappings from type variables to types.
    */
  class Substitution(val tvMap: Map[String, Type]) {
    /** Add the mapping `tvar` |-> `tpe` into the substitution.
      */
    def add(tvar: TypeVariable, tpe: Type): Substitution = {
      val subst1 = new Substitution(tvMap.updated(tvar.name, transformType(tpe)))
      new Substitution(subst1.tvMap map { case (k, v) => (k, subst1.transformType(v)) })
    }

    def subst(tv: TypeVariable): Type =
      tvMap get tv.name match {
        case None => tv
        case Some(tpe) => tpe
      }

    def transformType(tpe: Type): Type = tpe match {
      case x : GroundType.ArrayType => GroundType.ArrayType(transformType(x.itemType))
      case x : TypeVariable => subst(x)
      case LambdaType(paramTypes, valueType) =>
        LambdaType(paramTypes map transformType, transformType(valueType))
      case x => x
    }

    /** Instantiate type. Return `None` is there exists type variables that can not be instantiated.
      */
    def instantiateType(tpe: Type): Either[TypeVariable, Type] = tpe match {
      case x : GroundType.ArrayType => instantiateType(x.itemType) map { inst => GroundType.ArrayType(inst) }
      case x : TypeVariable => subst(x) match {
        case _ : TypeVariable => Left(x)
        case tpe => Right(tpe)
      }
      case LambdaType(paramTypes, valueType) =>
        @annotation.tailrec def recur(xs: List[Type], acc: List[Type]): Either[TypeVariable, List[Type]] = xs match {
          case Nil => Right(acc)
          case x :: xs => instantiateType(x) match {
            case Left(tv) => Left(tv)
            case Right(inst) => recur(xs, inst :: acc)
          }
        }

        for
          instParamTypes <- recur(paramTypes.reverse, Nil)
          instValueType <- instantiateType(valueType)
        yield
          LambdaType(instParamTypes, instValueType)
      case x => Right(x)
    }
    
    def transformConstr(constr: Constraint): Constraint =
      constr match {
        case e @ Equality(tpe1, tpe2, lhs, rhs) =>
          e.deriveEquality(transformType(tpe1), transformType(tpe2))
      }
    
    def apply(tpe: Type): Type = transformType(tpe)
    
    def apply(constr: Constraint): Constraint = transformConstr(constr)
  }

  object Substitution {
    /** Empty substitution.
      */
    def empty: Substitution = new Substitution(Map.empty)
  }

  /** Solver for constraints.
    */
  class ConstraintSolver {
    /** All constraints that have been recorded.
      */
    protected var constraints: List[Constraint] = Nil

    /** Cached constraints that have not been solved.
      */
    protected var cachedConstraints: List[Constraint] = Nil

    /** Solved substitution.
      */
    protected var solved: Substitution = Substitution.empty

    /** List all constraints.
      */
    def listConstraints: List[Constraint] = constraints

    /** Show all constraints.
      */
    def showConstraints: String =
      (
        constraints map {
          case Constraint.Equality(t1, t2, _, _) =>
            s"$t1 == $t2"
        } mkString "\n"
      )

    /** Add a `constr` into the constraints.
      */
    def addConstraint(constr: Constraint): Unit = {
      // record the constraint
      constraints = constr :: constraints
      // add the constraint to unsolved constraints
      cachedConstraints = constr :: cachedConstraints
    }

    /** Calls [[ConstraintSolver.addConstraint]](Equality(tpe1, tpe2)). It will check whether the equality is trivial.
      */
    def addEquality(tpe1: Type, tpe2: Type, pos: SourcePosSpan, lhs: Option[SourcePosSpan] = None, rhs: Option[SourcePosSpan] = None): Unit = {
      val subst = solve
      if subst.transformType(tpe1) != subst.transformType(tpe2) then {
        val eq = Equality(tpe1, tpe2, lhs, rhs).withPos(pos)
        addConstraint(eq.asInstanceOf[Constraint])
      }
    }

    /** Solve the constraints.
      */
    def solve: Substitution = {
      def tryInhertPredicates(tv1: TypeVariable, tp2: Type): Unit = tp2 match {
        case tv2 : TypeVariable =>
          tv2.mergePredicates(tv1)
        case cv2 : ClassTypeVariable =>
          cv2.predicates = cv2.predicates ++ tv1.predicates
        case _ if tv1.predicates.nonEmpty =>
          throw TypeError(s"can not instantiate type variable $tv1 with predicates to type $tp2")
        case _ =>
      }

      @annotation.tailrec def recur(constrs: List[Constraint], subst: Substitution): Substitution = {
        constrs match {
          case Nil => subst
          case constr :: xs => subst.transformConstr(constr) match {
            case Equality(tpe1, tpe2, _, _) if tpe1 == tpe2 => recur(xs, subst)
            case Equality(tpe1 : TypeVariable, tpe2, _, _) =>
              if tpe1 occursIn tpe2 then
                throw TypeError(s"can not solve recursive type $tpe1 ~ $tpe2")
              else {
                tryInhertPredicates(tpe1, tpe2)
                recur(xs, subst.add(tpe1, tpe2))
              }
            case Equality(tpe1, tpe2 : TypeVariable, _, _) =>
              if tpe2 occursIn tpe1 then
                throw TypeError(s"can not solve recursive type $tpe2 ~ $tpe1")
              else {
                tryInhertPredicates(tpe2, tpe1)
                recur(xs, subst.add(tpe2, tpe1))
              }
            case e @ Equality(GroundType.ArrayType(tpe1), GroundType.ArrayType(tpe2), _, _) =>
              recur(Equality(tpe1, tpe2).withPos(e).asInstanceOf[Constraint] :: xs, subst)
            case e @ Equality(LambdaType(p1, v1), LambdaType(p2, v2), _, _) =>
              if p1.length != p2.length then
                throw TypeError(s"can not unify lambda type with different parameter number: $p1 ~ $p2")
              else {
                val equalities = (v1 :: p1) zip (v2 :: p2) map { case (t1, t2) => Equality(t1, t2).withPos(e).asInstanceOf[Constraint] }
                recur(equalities ++ xs, subst)
              }
            case e : Equality =>
              throw TypeError(s"can not unify equality: $e").withPos(e)
          }
        }
      }

      // solve constraints incrementally and update the result
      solved = recur(cachedConstraints.reverse, solved)
      // clear unsolved constraints
      cachedConstraints = Nil

      solved
    }

    /** Alias for [[ConstraintSolver.solve]].
      */
    def subst: Substitution = solve
  }
}

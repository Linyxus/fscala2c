package fs2c.typer

import Types._
import fs2c.typer
import Typer.TypeError

object Constraints {

  /** Enum for constraints.
    */
  enum Constraint {
    case Equality(tpe1: Type, tpe2: Type)
    case ClassMemberType(tpe: Type, member: String, memberTpe: Type)
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
    
    def transformConstr(constr: Constraint): Constraint =
      constr match {
        case Equality(tpe1, tpe2) => 
          Equality(transformType(tpe1), transformType(tpe2))
        case ClassMemberType(tpe, member, memberType) => 
          ClassMemberType(transformType(tpe), member, transformType(memberType))
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
    protected var constraints: List[Constraint] = Nil
    
    def listConstraints: List[Constraint] = constraints
    
    def showConstraints: String =
      (
        constraints map {
          case Constraint.Equality(t1, t2) =>
            s"$t1 == $t2"
          case Constraint.ClassMemberType(tpe, member, memberType) =>
            s"$tpe <: { $member : $memberType }"
        } mkString "\n"
      )

    /** Add a `constr` into the constraints.
      */
    def addConstraint(constr: Constraint): Unit =
      constraints = constr :: constraints

    /** Alias for [[ConstraintSolver.addConstraint]](Equality(tpe1, tpe2)).
      */
    def addEquality(tpe1: Type, tpe2: Type): Unit =
      addConstraint(Equality(tpe1, tpe2))

    /** Solve the constraints.
      */
    def solve: Substitution = {
      @annotation.tailrec def recur(constrs: List[Constraint], subst: Substitution): Substitution = {
        constrs match {
          case Nil => subst
          case constr :: xs => subst.transformConstr(constr) match {
            case Equality(tpe1, tpe2) if tpe1 == tpe2 => recur(xs, subst)
            case Equality(tpe1 : TypeVariable, tpe2) =>
              if tpe1 occursIn tpe2 then
                throw TypeError(s"can not solve recursive type $tpe1 ~ $tpe2")
              else
                recur(xs, subst.add(tpe1, tpe2))
            case Equality(tpe1, tpe2 : TypeVariable) =>
              if tpe2 occursIn tpe1 then
                throw TypeError(s"can not solve recursive type $tpe2 ~ $tpe1")
              else
                recur(xs, subst.add(tpe2, tpe1))
            case Equality(GroundType.ArrayType(tpe1), GroundType.ArrayType(tpe2)) =>
              recur(Equality(tpe1, tpe2) :: xs, subst)
            case Equality(LambdaType(p1, v1), LambdaType(p2, v2)) =>
              if p1.length != p2.length then
                throw TypeError(s"can not unify lambda type with different parameter number: $p1 ~ $p2")
              else {
                val equalities = (v1 :: p1) zip (v2 :: p2) map { case (t1, t2) => Equality(t1, t2) }
                recur(equalities ++ xs, subst)
              }
            case e : Equality =>
              throw TypeError(s"can not unify $e\n$showConstraints")
            case ClassMemberType(tv : TypeVariable, member, memTpe) =>
              tv.predicates = Predicate.HaveMemberOfType(member, memTpe) :: tv.predicates
              recur(xs, subst)
            case ClassMemberType(tpe, _, _) =>
              throw TypeError(s"can not add class member predicate for type $tpe")
          }
        }
      }

      recur(constraints.reverse, Substitution.empty)
    }

    /** Alias for [[ConstraintSolver.solve]].
      */
    def subst: Substitution= solve
  }
}

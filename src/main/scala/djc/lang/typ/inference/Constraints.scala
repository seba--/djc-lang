package djc.lang.typ.inference

import djc.lang.Gensym
import djc.lang.typ.{SubstType, FreeTypeVars, Checker}
import djc.lang.typ.Checker.{TVarContext, subtype, join}
import djc.lang.typ.Types._
import djc.lang.typ.inference.TypePredicates._

/**
 * Constraint generation and solving, adapted from Pierce and Turner
 */
object Constraints {
  sealed trait Constraint {
    def min: Type
    def max: Type
  }
  case class Equal(tpe: Type) extends Constraint {
    def min = tpe
    def max = tpe
  }
  case class Between(lower: Type, upper: Type) extends Constraint  {
    def min = lower
    def max = upper
  }
  val unconstr: Constraint = Between(Bot, Top)

  def solve(tgamma: TVarContext)(tvar: Symbol)(c: Constraint, r: Type): Type = {
    if (isConstantIn(r, tvar) || isContravariantIn(r, tvar))
      c.min
    else if (isContravariantIn(r, tvar))
      c.max
    else if (isInvariantIn(r, tvar) && isTight(tgamma)(c))
      c.min
    else if (isRigidIn(r, tvar) && isRigid(tgamma, c))
      c.min
    else
      throw ConstraintsException(s"solution undefined for variable $tvar, constraint $c and type $r\n in context $tgamma")
  }

  def meet(tgamma: TVarContext)(c1: Constraint, c2: Constraint): Constraint = {
    (c1, c2) match {
      case (Equal(t1), Equal(t2)) if t1 === t2 => c1
      case (c@Equal(t1), Between(t2, t3)) if subtype(tgamma)(t2, t1) && subtype(tgamma)(t1, t3) => c
      case (Between(t2, t3), c@Equal(t1)) if subtype(tgamma)(t2, t1) && subtype(tgamma)(t1, t3) => c
      case (Between(t1, t2), Between(t3,t4)) => Between(join(tgamma)(t1,t3), Checker.meet(tgamma)(t2,t4))
    }
  }


  def isRigid(tgamma: TVarContext, c: Constraint): Boolean = c match {
    case Equal(t) => TypePredicates.isRigid(tgamma, t)
    case Between(t1, t2) if t1 === t2 => TypePredicates.isRigid(tgamma, t1)
    case _ => false
  }

  def isTight(tgamma: TVarContext)(c: Constraint): Boolean = c match {
    case Equal(t) => true
    case Between(t1, t2) =>
      Checker.subtype(tgamma)(t1, t2) && Checker.subtype(tgamma)(t2, t1)
  }

  object GenConstraints {
    //compute constraints on tvar which makes s a subtype of t
    def apply(tgamma: TVarContext)(tvar: Symbol)(s: Type, t: Type): Constraint = {
      val ftvs = FreeTypeVars(s)
      val ftvt = FreeTypeVars(t)

      if (tgamma.keySet(tvar))
        throw ConstraintsException(s"the unknown type variable $tvar is bound by context $tgamma")

      (ftvs(tvar), ftvs(tvar)) match {
        case (true, true) =>
          throw ConstraintsException(s"the unknown type variable $tvar may not occur in both arguments\n for ${(s, t)} with context $tgamma")
        case _ => subtypeConstraints(tgamma)(tvar)(s,t)
      }
    }

    //compute constraints on tvar which makes s a subtype of t
    //assumption: only one of s, t contains tvar and tvar not bound in tgamma
    private def subtypeConstraints(tgamma: TVarContext)(tvar: Symbol)(s: Type, t: Type): Constraint = (s, t) match {
      case (t1, Top) => unconstr
      case (Bot, t1) => unconstr
      case (TVar(`tvar`), t1) => Between(Bot, t1)
      case (TVar(a), TVar(b)) if a == b => unconstr
      case (TVar(a), t1) => subtypeConstraints(tgamma)(tvar)(tgamma(a), t1)
      case (t1, TVar(`tvar`)) => Between(t1, Top)

      case (TUniv(alpha1, bound1, t1), TUniv(alpha2, bound2, t2)) =>
        lazy val ftv1 = FreeTypeVars(t1)
        lazy val ftv2 = FreeTypeVars(t2)
        lazy val alphares =
          if (alpha1 == tvar || ( alpha1 != alpha2 && ftv2(alpha1)))
            Gensym(alpha1, ftv1 ++ ftv2 ++ tgamma.keySet + tvar)
          else alpha1

        val (t1res, t2res) =
          (SubstType(alpha1 -> TVar(alphares))(t1), SubstType(alpha2 -> TVar(alphares))(t2))
        val (boundres, k) = equalityConstraints(tgamma)(tvar)(bound1, bound2)
        val c = subtypeConstraints(tgamma + (alphares -> boundres))(tvar)(t1res, t2res)

        meet(tgamma)(k,c)

      case (TSvc(ts1), TSvc(ts2)) if ts1.length == ts2.length =>
        val css = (ts1 zip ts2).map { case (t1, t2) => subtypeConstraints(tgamma)(tvar)(t2, t1)  }
        css.foldLeft(unconstr)(meet(tgamma)(_,_))

      case (TSrvRep(svcs1), TSrvRep(svcs2)) if svcs2.keySet subsetOf svcs1.keySet =>
        val css = for(k <- svcs2.keys)
        yield subtypeConstraints(tgamma)(tvar)(svcs1(k), svcs2(k))
        css.foldLeft(unconstr)(meet(tgamma)(_,_))

      case (TSrv(t1), TSrv(t2)) =>
        subtypeConstraints(tgamma)(tvar)(t1, t2)

      case (TBase(name1, ts1), TBase(name2, ts2)) =>
        equalityConstraints(tgamma)(tvar)(s,t)._2
    }

    //compute constraints for tvar which make s,t equivalent + whichever of the two is concrete
    //assumption: only one of s, t contains tvar and tvar not bound in tgamma
    private def equalityConstraints(tgamma: TVarContext)(tvar: Symbol)(s: Type, t: Type): (Type, Constraint) = (s, t) match {
      case (Top, Top) => (Top, unconstr)
      case (Bot, Bot) => (Bot, unconstr)
      case (Unit, Unit) => (Unit, unconstr)
      case (TVar(`tvar`), t1) => (t1, Equal(t1))
      case (TVar(a), TVar(b)) if a == b => (s, unconstr)
      case (t1, TVar(`tvar`)) => (t1, Equal(t1))

      case (TUniv(alpha1, bound1, t1), TUniv(alpha2, bound2, t2)) =>
        lazy val ftv1 = FreeTypeVars(t1)
        lazy val ftv2 = FreeTypeVars(t2)
        lazy val alphares =
          if (alpha1 == tvar || ( alpha1 != alpha2 && ftv2(alpha1)))
            Gensym(alpha1, ftv1 ++ ftv2 ++ tgamma.keySet + tvar)
          else alpha1

        val (t1res, t2res) =
          (SubstType(alpha1 -> TVar(alphares))(t1), SubstType(alpha2 -> TVar(alphares))(t2))
        val (boundres, k) = equalityConstraints(tgamma)(tvar)(bound1, bound2)
        val (tres, c) = equalityConstraints(tgamma + (alphares -> boundres))(tvar)(t1res, t2res)

        (TUniv(alphares, boundres, tres), meet(tgamma)(k, c))

      case (TSvc(ts1), TSvc(ts2)) if ts1.length == ts2.length =>
        val (ts3, cs) = ((ts1 zip ts2).map { case (t1, t2) => equalityConstraints(tgamma)(tvar)(t1, t2)  }).unzip

        (TSvc(ts3), cs.foldLeft(unconstr)(meet(tgamma)(_,_)))

      case (TSrvRep(svcs1), TSrvRep(svcs2)) if svcs1.keySet == svcs2.keySet =>
        val x = for(k <- svcs1.keys)
        yield k -> equalityConstraints(tgamma)(tvar)(svcs1(k), svcs2(k))
        val tsres = x.toMap.mapValues(_._1)
        val c = x.unzip._2.unzip._2.foldLeft(unconstr)(meet(tgamma)(_,_))

        (TSrvRep(tsres), c)

      case (TSrv(t1), TSrv(t2)) =>
        val (tres, cs) = equalityConstraints(tgamma)(tvar)(t1, t2)

        (TSrv(tres), cs)

      case (TBase(name1, ts1), TBase(name2, ts2)) if name1 == name2 && ts1.length == ts2.length =>
        val (ts3, cs) = ((ts1 zip ts2).map { case (t1, t2) => equalityConstraints(tgamma)(tvar)(t1, t2)  }).unzip

        (TBase(name1, ts3), cs.foldLeft(unconstr)(meet(tgamma)(_,_)))
    }
  }

  case class ConstraintsException(msg: String) extends RuntimeException(msg)
}

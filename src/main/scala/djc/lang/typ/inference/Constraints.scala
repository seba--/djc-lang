package djc.lang.typ.inference

import djc.lang.Gensym
import djc.lang.typ.Types._
import djc.lang.typ.Checker
import djc.lang.typ.Checker.{TVarContext, subtype, join}
import djc.lang.typ.inference.TypePredicates._
import djc.lang.typ.inference.base.PairsOps._

/**
 * Constraint generation and solving, adapted from Pierce and Turner
 */
object Constraints {
  import op._
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

  type CSet = Map[Symbol, Constraint]
  val empty: CSet = Map().withDefaultValue(unconstr)
  object CSet {
    def apply(cs: (Symbol, Constraint)*): CSet = Map(cs: _*).withDefaultValue(unconstr)
  }

  def solve(tgamma: TVarContext, tvar: Symbol, c: Constraint, r: Type): Type = {
    if (isConstantIn(r, tvar) || isCovariantIn(r, tvar))
      c.min
    else if (isContravariantIn(r, tvar))
      c.max
    else if (isInvariantIn(r, tvar) && isTight(tgamma)(c)) //TODO: investigate why tightness is needed
      c.min
    else if (isRigidIn(r, tvar) && isRigid(tgamma, c))
      c.min
    else
      throw ConstraintsException(s"solution undefined for variable $tvar, constraint $c and type $r\n in context $tgamma")
  }

  def solve(tgamma: TVarContext, tvars: Set[Symbol], cs: CSet, r: Type): Map[Symbol, Type] = {
    val it = for(tv <- tvars)
      yield tv -> solve(tgamma, tv, cs(tv), r)
    it.toMap
  }

  def meet(tgamma: TVarContext, c1: Constraint, c2: Constraint): Constraint = {
    (c1, c2) match {
      case (Equal(t1), Equal(t2)) if t1 === t2 => c1
      case (c@Equal(t1), Between(t2, t3)) if subtype(tgamma)(t2, t1) && subtype(tgamma)(t1, t3) => c
      case (Between(t2, t3), c@Equal(t1)) if subtype(tgamma)(t2, t1) && subtype(tgamma)(t1, t3) => c
      case (Between(t1, t2), Between(t3,t4)) => Between(join(tgamma)(t1,t3), Checker.meet(tgamma)(t2,t4))
      case _ => throw ConstraintsException(s"meet undefined for $c1 and $c2\nunder $tgamma")
    }
  }

  def meet(tgamma: TVarContext, c1: CSet, c2: CSet): CSet = {
    val m = for{
             tv <- c1.keys ++ c2.keys;
             c = meet(tgamma, c1(tv), c2(tv))
             if c != unconstr }
      yield tv -> c

    m.toMap.withDefaultValue(unconstr)
  }

  def meet(tgamma: TVarContext, cs: Seq[CSet]): CSet = cs.foldLeft(CSet())(meet(tgamma, _, _))

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
    //compute constraints on tvars which makes s a subtype of t
    def apply(tgamma: TVarContext, tvars: Set[Symbol], s: Type, t: Type): CSet = {
      val ftvS = freeTypeVars(s)
      val ftvT = freeTypeVars(t)

      val intersect = tgamma.keySet.intersect(tvars)
      if (intersect.nonEmpty)
        throw ConstraintsException(s"the unknown type variables $intersect are bound in context $tgamma\n for $s -- $t with context $tgamma")


      ((ftvS intersect tvars).nonEmpty, (ftvT intersect tvars).nonEmpty) match {
        case (true, true) =>
          throw ConstraintsException(s"the unknown type variables $tvars occur in both arguments\n for ${(s, t)} with context $tgamma")
        case _ => subtypeConstraints(tgamma)(tvars)(s,t)
      }
    }

    def apply(tgamma: TVarContext, tvar: Symbol, s: Type, t: Type): Constraint = apply(tgamma, Set(tvar), s, t)(tvar)

    def apply(tgamma: TVarContext, tvars: Set[Symbol], ts: Seq[(Type, Type)]): CSet = {
      val cs = ts map { case (s, t) => apply(tgamma, tvars, s, t) }
      cs.foldLeft(CSet())(meet(tgamma, _, _))
    }

    //compute constraints on tvars which makes s a subtype of t
    //assumption: only one of s, t contains tvars and tvars not bound in tgamma
    private def subtypeConstraints(tgamma: TVarContext)(tvars: Set[Symbol])(s: Type, t: Type): CSet = (s, t) match {
      case (t1, Top) => CSet()
      case (Bot, t1) => CSet()
      case (TVar(a), TVar(b)) if a == b => CSet()
      case (TVar(tv), t1) if tvars(tv) => CSet(tv -> Between(Bot, t1))
      case (t1, TVar(tv)) if tvars(tv) => CSet(tv -> Between(t1, Top))
      case (TVar(a), t1) => subtypeConstraints(tgamma)(tvars)(tgamma(a), t1)

      case (TUniv(alpha1, bound1, t1), TUniv(alpha2, bound2, t2)) =>
        lazy val ftv1 = freeTypeVars(t1)
        lazy val ftv2 = freeTypeVars(t2)
        lazy val alphares =
          if (tvars(alpha1) || ( alpha1 != alpha2 && ftv2(alpha1)))
            Gensym(alpha1, ftv1 ++ ftv2 ++ tgamma.keySet ++ tvars)
          else alpha1

        val (t1res, t2res) =
          (substType(alpha1 -> TVar(alphares))(t1), substType(alpha2 -> TVar(alphares))(t2))
        val (boundres, k) = equalityConstraints(tgamma)(tvars)(bound1, bound2)
        val c = subtypeConstraints(tgamma + (alphares -> boundres))(tvars)(t1res, t2res)

        meet(tgamma, k, c)

      case (TSvc(ts1), TSvc(ts2)) if ts1.length == ts2.length =>
        val css = (ts1 zip ts2).map { case (t1, t2) => subtypeConstraints(tgamma)(tvars)(t2, t1)  }
        css.foldLeft(CSet())(meet(tgamma, _,_))

      case (TSrvRep(svcs1), TSrvRep(svcs2)) if svcs2.keySet subsetOf svcs1.keySet =>
        val css = for(k <- svcs2.keys)
        yield subtypeConstraints(tgamma)(tvars)(svcs1(k), svcs2(k))
        css.foldLeft(CSet())(meet(tgamma, _,_))

      case (TSrv(t1), TSrv(t2)) =>
        subtypeConstraints(tgamma)(tvars)(t1, t2)

      case (TPair(n, ts), TPair(n1, ts1)) if n1 <= n  =>
        meet(tgamma, ts.take(n1).zip(ts1) map {case (t,t1) => subtypeConstraints(tgamma)(tvars)(t,t1)})

      case (TBase(name1, ts1), TBase(name2, ts2)) if name1 == name2 =>
        equalityConstraints(tgamma)(tvars)(s,t)._2 //TODO this should be subtype constraints both ways
    }

    //compute constraints for tvar which make s,t equivalent + whichever of the two is concrete
    //assumption: only one of s, t contains tvar and tvar not bound in tgamma
    private def equalityConstraints(tgamma: TVarContext)(tvars: Set[Symbol])(s: Type, t: Type): (Type, CSet) = (s, t) match {
      case (Top, Top) => (Top, CSet())
      case (Bot, Bot) => (Bot, CSet())
      case (Unit, Unit) => (Unit, CSet())
      case (TVar(a), TVar(b)) if a == b => (s, CSet())
      case (TVar(tv), t1) if tvars(tv) => (t1, CSet(tv -> Equal(t1)))
      case (t1, TVar(tv)) if tvars(tv) => (t1, CSet(tv -> Equal(t1)))

      case (TUniv(alpha1, bound1, t1), TUniv(alpha2, bound2, t2)) =>
        lazy val ftv1 = freeTypeVars(t1)
        lazy val ftv2 = freeTypeVars(t2)
        lazy val alphares =
          if (tvars(alpha1) || ( alpha1 != alpha2 && ftv2(alpha1)))
            Gensym(alpha1, ftv1 ++ ftv2 ++ tgamma.keySet ++ tvars)
          else alpha1

        val (t1res, t2res) =
          (substType(alpha1 -> TVar(alphares))(t1), substType(alpha2 -> TVar(alphares))(t2))
        val (boundres, k) = equalityConstraints(tgamma)(tvars)(bound1, bound2)
        val (tres, c) = equalityConstraints(tgamma + (alphares -> boundres))(tvars)(t1res, t2res)

        (TUniv(alphares, boundres, tres), meet(tgamma, k, c))

      case (TSvc(ts1), TSvc(ts2)) if ts1.length == ts2.length =>
        val (ts3, cs) = ((ts1 zip ts2).map { case (t1, t2) => equalityConstraints(tgamma)(tvars)(t1, t2)  }).unzip

        (TSvc(ts3), cs.foldLeft(CSet())(meet(tgamma, _,_)))

      case (TSrvRep(svcs1), TSrvRep(svcs2)) if svcs1.keySet == svcs2.keySet =>
        val x = for(k <- svcs1.keys)
        yield k -> equalityConstraints(tgamma)(tvars)(svcs1(k), svcs2(k))
        val tsres = x.toMap.mapValues(_._1)
        val c = x.unzip._2.unzip._2.foldLeft(CSet())(meet(tgamma, _,_))

        (TSrvRep(tsres), c)

      case (TSrv(t1), TSrv(t2)) =>
        val (tres, cs) = equalityConstraints(tgamma)(tvars)(t1, t2)

        (TSrv(tres), cs)

      case (TBase(name1, ts1), TBase(name2, ts2)) if name1 == name2 && ts1.length == ts2.length =>
        val (ts3, cs) = ((ts1 zip ts2).map { case (t1, t2) => equalityConstraints(tgamma)(tvars)(t1, t2)  }).unzip

        (TBase(name1, ts3), cs.foldLeft(CSet())(meet(tgamma, _,_)))
    }
  }

  case class ConstraintsException(msg: String) extends RuntimeException(msg)
}

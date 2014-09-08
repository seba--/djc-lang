package djc.lang.typ.inference

import djc.lang.Gensym
import djc.lang.TypedSyntax.{Fold, Exp}
import djc.lang.typ._
import djc.lang.typ.Types._

object TypeInference {
  import Checker.Context
  import Checker.TVarContext

  //placeholder in partial type information
  case object Hole extends Type {
    override def ===(that: Type) = ???
  }
  //Variant of type substitution which works on partial types
  class SubstProtoType(substs: Map[Symbol, Type]) extends SubstType(substs) {
    override val mkSubst = SubstPrototype
    override def mapType: TMapT = {
      case Hole => Hole
      case t => super.mapType(t)
    }
  }
  object SubstPrototype extends SubstTypeFactory[SubstProtoType] {
    def apply(substs: Map[Symbol, Type]) = new SubstProtoType(substs.filter { case (k, TVar(v)) if k == v => false
    case _ => true})
  }

  object FreeProtoTypeVars extends FreeTypeVarsTemplate {
    override def foldType(init: Set[Symbol]): FoldT[Set[Symbol]] = {
      case Hole => init
      case tpe => super.foldType(init)(tpe)
    }
  }

  object IsPrototype extends Fold {
    def apply(t: Type): Boolean = foldType(false)(t)

    def foldType(init: Boolean): FoldT[Boolean] = {
      case Hole => true
      case t => super.foldType(init)(t)
    }
  }

  //can first arg be completed to second arg?
  def protoMatch(proto: Type, tpe: Type): Boolean = (proto, tpe) match {
    case (p,t) if !IsPrototype(p) => p === t
    case (Hole, t) => true
    case (TSvc(ps), TSvc(ts)) => ps.corresponds(ts)(protoMatch(_,_))
    case (TSrvRep(psvcs), TSrvRep(svcs)) if psvcs.keySet == svcs.keySet =>
      psvcs.forall {case (k,p) => protoMatch(p, svcs(k))}
    case (TSrv(p), TSrv(t)) =>
      protoMatch(p,t)
    case (TUniv(alpha1, bound1, p1), TUniv(alpha2, bound2, t2)) =>
      lazy val fptv = FreeProtoTypeVars(p1)
      lazy val ftv = FreeTypeVars(t2)
      val alphar =
        if (alpha1 != alpha2 && ftv(alpha1))
          Gensym(alpha1, fptv ++ ftv)
        else alpha1
      val p1res = SubstPrototype(alpha1 -> TVar(alphar))(p1)
      val t2res = SubstType(alpha2 -> TVar(alphar))(t2)

      protoMatch(bound1, bound2) && protoMatch(p1res, t2res)

    case (TBase(name1, ps), TBase(name2, ts)) if name1 == name2 =>
      ps.corresponds(ts)(protoMatch(_,_))
  }

  def infer(proto: Type, gamma: Context, tgamma: TVarContext, exp: Exp): Type = ???

  //compute greatest subtype of 1st arg which matches prototype in 2nd arg structurally
  def down(tpe: Type, proto: Type): Type = (tpe, proto) match {
    case (Hole, _) => throw InferenceException(s"wrong use of function, you provided a prototype where a type is expected")

    case (t, Hole) => t

    case (t, p) if !IsPrototype(p) && t === p => t

    case (_, Bot) => Bot

    case (Top, p) => p match {
      case TVar(alpha) => TVar(alpha)
      case Unit => Unit
      case TSvc(ts) => down(TSvc(List.fill(ts.length)(Bot)), p)
      case TSrvRep(svcs) => down(TSrvRep(), p)
      case TSrv(_) => down(TSrv(Top), p)
      case TUniv(alpha, bound, p2) => down(TUniv(alpha, bound, Top), p)
      case TBase(name, ts) if ts forall (!IsPrototype(_)) => TBase(name, ts)
    }

    case (TSvc(ts), TSvc(ps)) if ts.length == ps.length =>
      TSvc((ts zip ps) map {case (t,p) => up(t,p)})

    case (TSrvRep(svcs), TSrvRep(psvcs)) if svcs.keySet subsetOf psvcs.keySet =>
      TSrvRep(for((k,p) <- psvcs) yield k -> down(svcs.getOrElse(k, Top), p))

    case (TSrv(t1), TSrv(p1)) =>
      TSrv(down(t1, p1))

    case (TUniv(alpha, bound1, t), TUniv(beta, bound2, p)) if bound1 === bound2 =>
      lazy val fptv = FreeProtoTypeVars(p)
      lazy val ftv = FreeTypeVars(t)
      val alphar =
        if (alpha != beta && fptv(alpha))
          Gensym(alpha, fptv ++ ftv)
        else alpha
      val pres = SubstPrototype(beta -> TVar(alphar))(p)
      val tres = SubstType(alpha -> TVar(alphar))(t)

      TUniv(alphar, bound1, down(tres, pres))

    case (TBase(_), TBase(_)) if protoMatch(proto, tpe) => tpe
  }

  //compute least supertype of 1st arg which matches prototype in 2nd arg structurally
  def up(tpe: Type, proto: Type): Type = (tpe, proto) match {
    case (Hole, _) => throw InferenceException(s"wrong use of function, you provided a prototype where a type is expected")

    case (t, Hole) => t

    case (t, p) if t === p => t

    case (_, Top) => Top

    case (Bot, p) => p match {
      case TVar(alpha) => TVar(alpha)
      case Unit => Unit
      case TSvc(ts) => up(TSvc(List.fill(ts.length)(Top)), p)
      case TSrvRep(svcs) => up(TSrvRep(svcs.mapValues(x => Bot)), p)
      case TSrv(_) => up(TSrv(Bot), p)
      case TUniv(alpha, bound, p2) => up(TUniv(alpha, bound, Bot), p)         //TODO what about the bounds?
      case TBase(name, ts) if ts forall (!IsPrototype(_)) => TBase(name, ts)
    }

    case (TSvc(ts), TSvc(ps)) if ts.length == ps.length =>
      TSvc((ts zip ps) map {case (t,p) => down(t,p)})

    case (TSrvRep(svcs), TSrvRep(psvcs)) if psvcs.keySet subsetOf svcs.keySet =>
      TSrvRep(for((k,p) <- psvcs) yield k -> up(svcs(k), p))

    case (TSrv(t1), TSrv(p1)) =>
      TSrv(up(t1, p1))

    case (TUniv(alpha, bound1, t), TUniv(beta, bound2, p)) if bound1 === bound2 =>
      lazy val fptv = FreeProtoTypeVars(p)
      lazy val ftv = FreeTypeVars(t)
      val alphar =
        if (alpha != beta && fptv(alpha))
          Gensym(alpha, fptv ++ ftv)
        else alpha
      val pres = SubstPrototype(beta -> TVar(alphar))(p)
      val tres = SubstType(alpha -> TVar(alphar))(t)

      TUniv(alphar, bound1, up(tres, pres))

    case (TBase(_), TBase(_)) if protoMatch(proto, tpe) => tpe
  }

  //Constraint generation and solving, adapted from Pierce and Turner
  sealed trait Constraint
  case class Equal(tpe: Type) extends Constraint
  case class Between(lower: Type, upper: Type) extends Constraint
  val unconstr: Constraint = Between(Bot, Top)

  def meet(tgamma: TVarContext)(c1: Constraint, c2: Constraint): Constraint = {
    import Checker.subtype
    (c1, c2) match {
      case (Equal(t1), Equal(t2)) if t1 === t2 => c1
      case (c@Equal(t1), Between(t2, t3)) if subtype(tgamma)(t2, t1) && subtype(tgamma)(t1, t3) => c
      case (Between(t2, t3), c@Equal(t1)) if subtype(tgamma)(t2, t1) && subtype(tgamma)(t1, t3) => c
      case (Between(t1, t2), Between(t3,t4)) => Between(Checker.join(tgamma)(t1,t3), Checker.meet(tgamma)(t2,t4))
    }
  }

  object GenConstraints {
    //compute constraints on tvar which makes s a subtype of t
    def apply(tgamma: TVarContext)(tvar: Symbol)(s: Type, t: Type): Constraint = {
      val ftvs = FreeTypeVars(s)
      val ftvt = FreeTypeVars(t)

      (ftvs(tvar), ftvs(tvar)) match {
        case (true, true) =>
          throw InferenceException(s"the unknown type variable $tvar may not occur in both arguments\n for ${(s, t)} with context $tgamma")
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

  case class InferenceException(msg: String) extends RuntimeException(msg)
}

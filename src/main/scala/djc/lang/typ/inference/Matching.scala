package djc.lang.typ.inference

import djc.lang.Gensym
import djc.lang.typ.{SubstType, FreeTypeVars}
import djc.lang.typ.Types._


/**
 * Matching ops for partial type information
 */
object Matching {
  import ProtoTypes._

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

  //compute greatest subtype of 1st arg which matches prototype in 2nd arg structurally
  def down(tpe: Type, proto: Type): Type = (tpe, proto) match {
    case (Hole, _) => throw MatchException(s"wrong use of function, you provided a prototype where a type is expected")

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

    case (TBase(name1, _), TBase(name2, _)) if name1 == name2 && protoMatch(proto, tpe) => tpe
  }

  //compute least supertype of 1st arg which matches prototype in 2nd arg structurally
  def up(tpe: Type, proto: Type): Type = (tpe, proto) match {
    case (Hole, _) => throw MatchException(s"wrong use of function, you provided a prototype where a type is expected")

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

    case (TBase(name1, _), TBase(name2, _)) if name1 == name2 && protoMatch(proto, tpe) => tpe
  }

  case class MatchException(msg: String) extends RuntimeException(msg)
}

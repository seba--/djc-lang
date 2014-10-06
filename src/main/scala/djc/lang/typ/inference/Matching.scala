package djc.lang.typ.inference

import djc.lang.typ.Checker.promote
import djc.lang.typ.Checker.TVarContext
import djc.lang.typ.TypeOps
import djc.lang.typ.{ Types => TP }
import djc.lang.typ.inference.{ ProtoTypes => PT }
//import util.ExceptionContext


/**
 * Matching ops for partial type information
 */
object Matching {
  type PType = PT.Type

  import PT.op.isPrototype
  import TP._
  
  implicit class TypeMatchingOps(val t: Type) extends AnyVal {
    def matchDown(proto: PType)(implicit tgamma: TVarContext): Type = Matching.down(t, proto)
    def matchUp(proto: PType)(implicit tgamma: TVarContext): Type = Matching.up(t, proto)
    def protoMatch(tpe: PType): Boolean = Matching.protoMatch(tpe, t)
  }

  //can first arg be completed to second arg?
  def protoMatch(proto: PType, tpe: Type): Boolean = (proto, tpe) match {
    case (p,t) if !isPrototype(p) => p.toFamily(TP) === t
    case (PT.Hole, t) => true
    case (PT.TSvc(ps), TSvc(ts)) => ps.corresponds(ts)(protoMatch(_,_))
    case (PT.TSrvRep(psvcs), TSrvRep(svcs)) if psvcs.keySet == svcs.keySet =>
      psvcs.forall {case (k,p) => protoMatch(p, svcs(k))}
    case (PT.TSrv(p), TSrv(t)) =>
      protoMatch(p,t)
    case (PT.TUniv(alpha1, bound1, p1), TUniv(alpha2, bound2, t2)) =>
      val (_, t2res, p1res) = TypeOps.captureAvoiding(TP, PT)(alpha2, t2, alpha1, p1)

      protoMatch(bound1, bound2) && protoMatch(p1res, t2res)

    case (PT.TBase(name1, ps), TBase(name2, ts)) if name1 == name2 =>
      ps.corresponds(ts)(protoMatch(_,_))

    case _ => false
  }

  //compute greatest subtype of 1st arg which matches prototype in 2nd arg structurally
  def down(tpe: Type, proto: PType)(implicit tgamma: TVarContext): Type = (tpe, proto) match {
    case (t, PT.Hole) => t

    case (TVar(alpha), PT.TVar(beta)) if alpha == beta => tpe

    case (TVar(alpha), _) => {
      val alphaSubs = (tgamma.filter { case (_,TVar(`alpha`)) => true }).keySet
      alphaSubs.size match {
        case 0 => down(Bot, proto)
        case 1 => down(alphaSubs.head, proto)
      }
    }

    case (t, p) if !isPrototype(p) && t === p.toFamily(TP) => t

    case (_, PT.Bot) => Bot

    case (Top, p) => p match {
      case PT.TVar(alpha) => TVar(alpha)
      case PT.Unit => Unit
      case PT.TSvc(ts) => down(TSvc(List.fill(ts.length)(Bot)), p)
      case PT.TSrvRep(svcs) => down(TSrvRep(), p)
      case PT.TSrv(_) => down(TSrv(Top), p)
      case PT.TUniv(alpha, bound, p2) => down(TUniv(alpha, bound.toFamily(TP), Top), p)
      case PT.TPair(n, ps) => down(TPair(Seq.fill(n)(Top):_*), p)
      case PT.TBase(name, ts) if ts forall (!isPrototype(_)) => p.toFamily(TP)
    }

    case (TSvc(ts), PT.TSvc(ps)) if ts.length == ps.length =>
      TSvc((ts zip ps) map {case (t,p) => up(t,p)})

    case (TSrvRep(svcs), PT.TSrvRep(psvcs)) if svcs.keySet subsetOf psvcs.keySet =>
      TSrvRep(for((k,p) <- psvcs) yield k -> down(svcs.getOrElse(k, Top), p))

    case (TSrv(t1), PT.TSrv(p1)) =>
      TSrv(down(t1, p1))

    case (TUniv(alpha, bound1, t), PT.TUniv(beta, bound2, p)) if bound1 === bound2.toFamily(TP) =>
      val (alphar, tres, pres) = TypeOps.captureAvoiding(TP, PT)(alpha, t, beta, p)
      TUniv(alphar, bound1, down(tres, pres)(tgamma + (alphar -> bound1)))

    case (TPair(n, ts), PT.TPair(n1, ps)) if n <= n1 =>
      TPair(((ts zip ps.take(n)) map {case (t,p) => up(t,p)}) ++ (ps.drop(n).map(down(Top, _))):_*)

    case (TBase(name1, _), PT.TBase(name2, _)) if name1 == name2 && protoMatch(proto, tpe) => tpe

    case _ => throw MatchException(s"matchDown failed for $tpe -- $proto\nin context $tgamma")
  }

  //compute least supertype of 1st arg which matches prototype in 2nd arg structurally
  def up(tpe: Type, proto: PType)(implicit tgamma: TVarContext): Type = (tpe, proto) match {
    case (t, PT.Hole) => t

    case (TVar(alpha), PT.TVar(beta)) if alpha == beta => tpe

    case (TVar(alpha), _) => up(tgamma(alpha), proto)

    case (t, p) if !isPrototype(p) && t === p.toFamily(TP) => t

    case (_, PT.Top) => Top

    case (Bot, p) => p match {
      case PT.TVar(alpha) => TVar(alpha)
      case PT.Unit => Unit
      case PT.TSvc(ts) => up(TSvc(List.fill(ts.length)(Top)), p)
      case PT.TSrvRep(svcs) => up(TSrvRep(svcs.mapValues(x => Bot)), p)
      case PT.TSrv(_) => up(TSrv(Bot), p)
      case PT.TUniv(alpha, bound, p2) => up(TUniv(alpha, bound.toFamily(TP), Bot), p)         //TODO what about the bounds?
      case PT.TPair(n, ps) => up(TPair(Seq.fill(n)(Bot):_*), p)
      case PT.TBase(name, ts) if ts forall (!isPrototype(_)) => p.toFamily(TP)
    }

    case (TSvc(ts), PT.TSvc(ps)) if ts.length == ps.length =>
      TSvc((ts zip ps) map {case (t,p) => down(t,p)})

    case (TSrvRep(svcs), PT.TSrvRep(psvcs)) if psvcs.keySet subsetOf svcs.keySet =>
      TSrvRep(for((k,p) <- psvcs) yield k -> up(svcs(k), p))

    case (TSrv(t1), PT.TSrv(p1)) =>
      TSrv(up(t1, p1))

    case (TUniv(alpha, bound1, t), PT.TUniv(beta, bound2, p)) if bound1 === bound2.toFamily(TP) =>
      val (alphar, tres, pres) = TypeOps.captureAvoiding(TP, PT)(alpha, t, beta, p)
      TUniv(alphar, bound1, up(tres, pres)(tgamma + (alphar -> bound1)))

    case (TPair(n, ts), PT.TPair(n1, ps)) if n1 <= n =>
      TPair((ts.take(n1) zip ps) map {case (t,p) => up(t,p)}:_*)

    case (TBase(name1, _), PT.TBase(name2, _)) if name1 == name2 && protoMatch(proto, tpe) => tpe

    case _ => throw MatchException(s"matchUp failed for $tpe -- $proto\nin context $tgamma")
  }

  case class MatchException(msg: String) extends RuntimeException(msg)
}

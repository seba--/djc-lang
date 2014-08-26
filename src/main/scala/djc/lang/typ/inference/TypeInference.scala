package djc.lang.typ.inference

import djc.lang.TypedSyntax.{Fold, Exp}
import djc.lang.typ.{SubstType, SubstTypeFactory, SubstTypeTemplate, Checker}
import djc.lang.typ.Types._


object TypeInference {
  import Checker.Context
  import Checker.TVarContext

  //placeholder in partial type information
  case object Hole extends Type {
    override def ===(that: Type) = ???
  }
  class SubstProtoType(substs: Map[Symbol, Type]) extends SubstType(substs) {
    override val mkSubst = SubstPrototype
    override def mapType: TMapT = {
      case Hole => Hole
      case t => super.mapType(t)
    }
  }
  object SubstPrototype extends SubstTypeFactory[SubstProtoType] {
    def apply(substs: Map[Symbol, Type]) = new SubstProtoType(substs)
  }

  object IsPrototype extends Fold {
    def apply(t: Type): Boolean = foldType(false)(t)

    def foldType(init: Boolean): FoldT[Boolean] = {
      case Hole => true
      case t => super.foldType(init)(t)
    }
  }

  def infer(proto: Type, gamma: Context, tgamma: TVarContext, exp: Exp): Type = ???

  //compute greatest subtype of 1st arg which matches type in 2nd arg structurally
  def down(tpe: Type, proto: Type): Type = (tpe, proto) match {
    case (Hole, _) => throw InferenceException(s"wrong use of function, you provided a prototype where a type is expected")

    case (t, Hole) => t

    case (t, p) if t === p => t

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

    case (TSvc(ts), TSvc(ps)) if ts.length == ps.length =>  //TODO correct?
      TSvc((ts zip ps) map {case (t,p) => up(t,p)})

    case (TSrvRep(svcs), TSrvRep(psvcs)) if svcs.keySet subsetOf psvcs.keySet =>
      TSrvRep(for((k,p) <- psvcs) yield k -> down(svcs.getOrElse(k, Top), p))

    case (TSrv(t1), TSrv(p1)) =>
      TSrv(down(t1, p1))

    case (TUniv(alpha, bound1, t), TUniv(beta, bound2, p)) if bound1 === bound2 =>
      TUniv(alpha, bound1, down(t, SubstPrototype(beta -> TVar(alpha))(p)))
  }

  //compute least supertype of 1st arg which matches type in 2nd arg structurally
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

    case (TSvc(ts), TSvc(ps)) if ts.length == ps.length => //TODO
      TSvc((ts zip ps) map {case (t,p) => down(t,p)})

    case (TSrvRep(svcs), TSrvRep(psvcs)) if psvcs.keySet subsetOf svcs.keySet =>
      TSrvRep(for((k,p) <- psvcs) yield k -> up(svcs(k), p))

    case (TSrv(t1), TSrv(p1)) =>
      TSrv(up(t1, p1))

    case (TUniv(alpha, bound1, t), TUniv(beta, bound2, p)) if bound1 === bound2 =>
      TUniv(alpha, bound1, up(t, SubstPrototype(beta -> TVar(alpha))(p)))

    //TODO control variance in TBase?

  }

  case class InferenceException(msg: String) extends RuntimeException(msg)

}

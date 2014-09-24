package djc.lang.typ.inference

import djc.lang.Gensym
import djc.lang.{TypedLanguage => TS}
import djc.lang.typ._
import djc.lang.typ.inference.{ExtLanguage => XS, ProtoTypes => PT}

import scala.collection.immutable.ListMap

object TypeInference {
  import Constraints._
  import Matching._
  import XS._
  import Checker._
  
  type XExp = XS.Exp
  type Exp = TS.Exp
  type PType = PT.Type
  type Type = TS.types.Type

  import TS.types._


  def infer(proto: PType, gamma: Context, tgamma: TVarContext, exp: XExp): (Type, Exp) = exp match {
    case Var(x) =>
      (gamma(x) matchUp proto, exp.toFamily(TS))

    case Par(ps) => proto match {
      case PT.Hole | PT.Unit | PT.Top =>
        val (psts, pses) = ps.map(infer(PT.Unit, gamma, tgamma, _)).unzip
        val joined = psts.foldLeft(Unit.asInstanceOf[Type]) { (t, u) => join(tgamma)(t, u)}
        val tres = if (proto == PT.Hole) PT.Unit else proto

        (joined matchUp tres, TS.Par(pses))
    }

    case Send(rcv, args) => proto match {
      case PT.Hole | PT.Unit | PT.Top =>
        val (t@TSvc(ts), rcve) = infer(PT.Hole, gamma, tgamma, rcv)
        if (ts.length != args.length)
          throw InferenceException(s"receiver/arg counts do not match: inferred $t for rcv\n with arguments $args")
        val (_, arges) = (ts zip args).map { case (t, e) => infer(t.toFamily(PT), gamma, tgamma, e)}.unzip
        val tres = if (proto == PT.Hole) Unit else proto.toFamily(TS.types)

        (tres, TS.Send(rcve, arges))

      case p =>
        val (Bot, rcve) = infer(PT.Hole, gamma, tgamma, rcv)
        val (_, arges) = args.map(infer(PT.Top, gamma, tgamma, _)).unzip

        (Bot matchUp p, TS.Send(rcve, arges))
    }

    case ServiceRef(srv, x) =>
      val (TSrv(TSrvRep(svcs)), srve) = infer(PT.TSrv(PT.TSrvRep(x -> proto)), gamma, tgamma, srv)
      (svcs(x), TS.ServiceRef(srve, x))

    case srv@ServerImpl(rules) =>
      if (!(freeTypeVars(srv.signature) subsetOf tgamma.keySet))
        throw InferenceException(s"Illegal free type variables: ${freeTypeVars(srv.signature) -- tgamma.keySet}")

      val (_, irules) = rules.map(inferRule(gamma, tgamma, _, srv.signature)).unzip
      proto match {
        case PT.Hole | PT.Top | PT.TSrvRep(_) =>
          (srv.signature matchUp proto, TS.ServerImpl(irules))
        case _ =>
          throw InferenceException(s"Could not match expression $srv with prototype $proto")
      }

    case srv@UServerImpl(rules) => proto match {
      case PT.TSrvRep(psvcs) =>
        if (PT.isPrototype(proto))
          throw InferenceException(s"Type of server must be fully known from the usage context, got incomplete type $proto")
        if (!(psvcs.keySet subsetOf srv.services.keySet))
          throw InferenceException(s"Untyped server $srv is incompatible with prototype $proto")

        val tsrv@TSrvRep(svcs)  = proto.toFamily(TS.types)
        
        //assume services not mentioned in proto to be of the most general service type
        val added = for((svc, arity) <- srv.services if !svcs.contains(svc))
          yield svc -> TSvc(List.fill(arity)(Bot))
        val tsrv2 = TSrvRep(svcs ++ added)

        val (_, irules) = rules.map(inferRule(gamma, tgamma, _, tsrv2)).unzip
        (tsrv, TS.ServerImpl(irules))
    }

    case Spawn(local, e) =>
      proto match {
        case PT.Hole | PT.Top =>
          val (tsrv@TSrvRep(_), ie) = infer(PT.Hole, gamma, tgamma, e)
          (TSrv(tsrv) matchUp proto, TS.Spawn(local, ie))

        case PT.TSrv(p@PT.TSrvRep(_)) =>
          val (t, ie) = infer(p, gamma, tgamma, e)
          (TSrv(t), TS.Spawn(local, ie))

        case p =>
          val (Bot, ie) = infer(PT.Hole, gamma, tgamma, e)
          (Bot matchUp p, TS.Spawn(local, ie))
      }

    //TODO Bot case
    case TApp(e, t) =>
      val (TUniv(alpha, bound, t2), ie) = infer(PT.Hole, gamma, tgamma, e)
      if (!subtype(tgamma)(t, bound))
        throw InferenceException(s"Type application of $t does not satisfy inferred bound $bound")
      (substType(alpha -> t)(t2), ie)

    //TODO Bot case
    case UTApp(e) =>
      val (TUniv(alpha, bound, t), ie) = infer(PT.Hole, gamma, tgamma, e)
      val c1 = GenConstraints(tgamma, alpha, t, Top matchDown proto)
      val c2 = GenConstraints(tgamma, alpha, TVar(alpha), bound)
      val subst = solve(tgamma, alpha, Constraints.meet(tgamma, c1, c2), t)
      val sigma = substType(alpha -> subst)
      val tres = sigma(t)
      (tres matchUp proto, TS.TApp(ie, subst))

    case TAbs(alpha, bound, e) =>
      proto match {
      	case PT.Hole | PT.Top =>
       
      	  val (t, ie) = infer(proto, gamma, tgamma + (alpha -> bound), e)
      	  (TUniv(alpha, bound, t), TS.TAbs(alpha, bound, ie))

      	case PT.TUniv(alpha2, bound2, p) if !PT.isPrototype(bound2) && bound === bound2.toFamily(TS.types) =>
        lazy val fptv = PT.freeTVars(p)
        val alphares =
          if (alpha != alpha2 && fptv(alpha))
            Gensym(alpha, freeTypeVars(e) ++ fptv)
          else alpha

        val (te, ie) = infer(PT.substT(alpha2 -> PT.TVar(alphares))(p),
          gamma, tgamma + (alphares -> bound),
          substType(alpha -> TVar(alphares))(e))
        (TUniv(alphares, bound, te), TS.TAbs(alphares, bound, ie))
    }

    case UnsafeCast(e, t) =>
      val (_, ie) = infer(PT.Hole, gamma, tgamma, e)
      (t matchUp proto, TS.UnsafeCast(ie, t))

    case UpCast(e, t) =>
      val (_, ie) = infer(t.toFamily(PT), gamma, tgamma, e)
      (t matchUp proto, TS.UpCast(ie, t))

    case BaseCall(b, Nil, es) =>
      //infer type arguments
      val (tvarslist, bounds) = b.targs.unzip  //TODO baseops are also type binders, hence we need to ensure the bound vars are not in scope already
      val tvars = tvarslist.toSet
      val substTargs = PT.substT(tvarslist zip List.fill(tvarslist.length)(PT.Hole))
      val (argTypes, inferredArgs) = ((b.ts zip es) map { case (t, e) => infer(substTargs(t.toFamily(PT)), gamma, tgamma, e)}).unzip

      val cargs = GenConstraints(tgamma, tvars, argTypes zip b.ts)
      val cresult = GenConstraints(tgamma, tvars, b.res, Top matchDown proto)
      //TODO forbid interdependent bounds
      val cbounds = GenConstraints(tgamma, tvars, tvarslist.map(TVar(_)) zip bounds)
      val call = Constraints.meet(tgamma, Seq(cargs, cresult, cbounds))
      val sigma = substType(solve(tgamma, tvars, call, b.res))

      (sigma(b.res) matchUp proto, TS.BaseCall(b.toFamily(TS), tvarslist map (s => sigma(TVar(s))), inferredArgs))

    case BaseCall(b, ts, es) =>
      if (b.targs.length != ts.length)
        throw InferenceException(s"Insufficient type arguments ($ts) provided for baseop $b.")

      val (tArgs, bounds) = b.targs.unzip
      if (!ts.corresponds(bounds)(subtype(tgamma)(_, _)))
        throw InferenceException(s"Type arguments do not match bounds of type parameters. Applied $ts to ${b.targs}\n in $exp")

      val sigma: Type => Type = substType(tArgs zip ts)(_)
      val bSig = b.ts map sigma

      //no need to bind the synthesized types, since in this case, they exactly match b.ts
      val (_, infEs) = ((b.ts zip es) map { case (p, e) => infer(p.toFamily(PT), gamma, tgamma, e)}).unzip
      (sigma(b.res) matchUp proto, TS.BaseCall(b.toFamily(TS), ts, infEs))

    case _ => throw InferenceException(s"Cannot infer type for $exp. Unknown syntactic form.")
  }

  def inferRule(gamma: Context, tgamma: TVarContext, r: Rule, srvSignature: TSrvRep): (Type, TS.Rule) = {
    val ruleGamma = gamma ++ r.rcvars + ('this -> TSrv(srvSignature))
    val (t, body) = infer(PT.Unit, ruleGamma, tgamma, r.p)
    (t, TS.Rule(r.ps map {_.toFamily(TS)}, body))
  }

  def inferRule(gamma: Context, tgamma: TVarContext, r: URule, srvSignature: TSrvRep): (Type, TS.Rule) = {
    val typedps = r.ps map { p =>
      srvSignature.svcs(p.name) match {
        case TSvc(ts) if ts.length == p.params.length =>
          val args = ListMap((p.params zip ts):_*)
          TS.Pattern(p.name, args)
      }
    }
    val rcvars = TS.Rule(typedps, TS.Par()).rcvars

    val ruleGamma = gamma ++ rcvars + ('this -> TSrv(srvSignature))
    val (t, body) = infer(PT.Unit, ruleGamma, tgamma, r.p)
    (t, TS.Rule(typedps, body))
  }

  case class InferenceException(msg: String) extends RuntimeException(msg)
}

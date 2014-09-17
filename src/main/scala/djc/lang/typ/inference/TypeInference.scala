package djc.lang.typ.inference

import djc.lang.Gensym
import djc.lang.TypedSyntax._
import djc.lang.typ._
import djc.lang.typ.Types._

import scala.collection.immutable.ListMap

object TypeInference {
  import Checker._
  import ProtoTypes._
  import Constraints._
  import Matching._
  import ExtSyntax._

  def infer(proto: Type, gamma: Context, tgamma: TVarContext, exp: Exp): (Type, Exp) = exp match {
    case Var(x) =>
      (gamma(x) matchUp proto, exp)

    case Par(ps) => proto match {
      case Hole | Unit | Top =>
        val (psts, pses) = ps.map(infer(Unit, gamma, tgamma, _)).unzip
        val joined = psts.foldLeft(Unit.asInstanceOf[Type]) { (t,u) => join(tgamma)(t,u)}
        val tres = if (proto == Hole) Unit else proto

        (joined matchUp tres, Par(pses))
    }

    case Send(rcv, args) => proto match {
      case Hole | Unit | Top =>
        val (t@TSvc(ts), rcve) = infer(Hole, gamma, tgamma, rcv)
        if (ts.length != args.length)
          throw InferenceException(s"receiver/arg counts do not match: inferred $t for rcv\n with arguments $args")
        val (_, arges) = (ts zip args).map { case (t, e) => infer(t, gamma, tgamma, e)    }.unzip
        val tres = if (proto == Hole) Unit else proto

        (tres, Send(rcve, arges))

      case p =>
        val (Bot, rcve) = infer(Hole, gamma, tgamma, rcv)
        val (_, arges) = args.map(infer(Top, gamma, tgamma, _)).unzip

        (Bot matchUp p, Send(rcve, arges))
    }

    case ServiceRef(srv, x) =>
      val (TSrv(TSrvRep(svcs)), srve) = infer(TSrv(TSrvRep(x -> proto)), gamma, tgamma, srv)
      (svcs(x), ServiceRef(srve, x))

    case srv@ServerImpl(rules) =>
      if (!(FreeTypeVars(srv.signature) subsetOf tgamma.keySet))
        throw InferenceException(s"Illegal free type variables: ${FreeTypeVars(srv.signature) -- tgamma.keySet}")

      val (_, irules) = rules.map(inferRule(gamma, tgamma, _, srv.signature)).unzip
      proto match {
        case Hole | Top | TSrvRep(_) =>
          (srv.signature matchUp proto, ServerImpl(irules))
        case _ =>
          throw InferenceException(s"Could not match expression $srv with prototype $proto")
      }

    case srv@UServerImpl(rules) => proto match {
      case tsrv@TSrvRep(psvcs) =>
        if (IsPrototype(proto))
          throw InferenceException(s"Type of server must be fully known from the usage context, got incomplete type $proto")
        if (!(psvcs.keySet subsetOf srv.services.keySet))
          throw InferenceException(s"Untyped server $srv is incompatible with prototype $proto")

        //TODO need to expand prototype with services which are added by srv

        val (_, irules) = rules.map(inferRule(gamma, tgamma, _, tsrv)).unzip
        (proto, ServerImpl(irules))
    }

    case Spawn(local, e) =>
      proto match {
        case Hole | Top  =>
          val (tsrv@TSrvRep(_), ie) = infer(proto, gamma, tgamma, e)
          (TSrv(tsrv) matchUp proto, Spawn(local, ie))

        case TSrv(p@TSrvRep(_)) =>
          val (t, ie) = infer(p, gamma, tgamma, e)
          (TSrv(t), Spawn(local, ie))

        case p =>
          val (Bot, ie) = infer(Hole, gamma, tgamma, e)
          (Bot matchUp p, Spawn(local, ie))
      }

    //TODO Bot case
    case TApp(e, t) =>
      val (TUniv(alpha, bound, t2), ie) = infer(Hole, gamma, tgamma, e)
      if (!subtype(tgamma)(t, bound))
        throw InferenceException(s"Type application of $t does not satisfy inferred bound $bound")
      (SubstType(alpha -> t)(t2), ie)

    //TODO Bot case
    case UTApp(e) =>
      val (TUniv(alpha, bound, t), ie) = infer(Hole, gamma, tgamma, e)
      val c1 = GenConstraints(tgamma, alpha, t, Top matchDown proto)
      val c2 = GenConstraints(tgamma, alpha, TVar(alpha), bound)
      val subst = solve(tgamma, alpha, Constraints.meet(tgamma, c1, c2), t)
      val sigma = SubstType(alpha -> subst)
      val tres = sigma(t)
      (tres matchUp proto, TApp(ie, subst))

    case TAbs(alpha, bound, e) => proto match {
      case Hole | Top =>
        infer(proto, gamma, tgamma + (alpha -> bound), e)

      case TUniv(alpha2, bound2, p) if !IsPrototype(bound2) && bound === bound2 =>
        lazy val fptv = FreeProtoTypeVars(p)
        val alphares =
          if (alpha != alpha2 && fptv(alpha))
            Gensym(alpha, FreeTypeVars(e) ++ fptv)
          else alpha

        val (te, ie) = infer(SubstPrototype(alpha2 -> TVar(alphares))(p),
          gamma, tgamma + (alphares -> bound),
          SubstType(alpha -> TVar(alphares))(e))
        (TUniv(alphares, bound, te), TAbs(alphares, bound, ie))
    }

    case UnsafeCast(e, t) => ???
    case UpCast(e, t) => ???

    case BaseCall(b, Nil, es) =>
      //infer type arguments
      val (tvarslist, bounds) = b.targs.unzip
      val tvars = tvarslist.toSet
      val substTargs = SubstPrototype(tvarslist zip List.fill(tvarslist.length)(Hole))
      val (argTypes, inferredArgs) = ((b.ts zip es) map { case (t, e) => infer(substTargs(t), gamma, tgamma, e) }).unzip

      val cargs = GenConstraints(tgamma, tvars, argTypes zip b.ts)
      val cresult = GenConstraints(tgamma, tvars, b.res, Top matchDown proto)
      //TODO forbid interdependent bounds
      val cbounds = GenConstraints(tgamma, tvars, tvarslist.map(TVar(_)) zip bounds)
      val call = Constraints.meet(tgamma, Seq(cargs, cresult, cbounds))
      val sigma = SubstType(solve(tgamma, tvars, call, b.res))

      (sigma(b.res) matchUp proto, BaseCall(b, tvarslist map (s => sigma(TVar(s))), inferredArgs))

    case BaseCall(b, ts, es) =>
      if (b.targs.length != ts.length)
        throw InferenceException(s"Insufficient type arguments ($ts) provided for baseop $b.")

      val (tArgs, bounds) = b.targs.unzip
      if (!ts.corresponds(bounds)(subtype(tgamma)(_,_)))
        throw InferenceException(s"Type arguments do not match bounds of type parameters. Applied $ts to $b.targs\n in $exp")

      val sigma: Type => Type = SubstType(tArgs zip ts)(_)
      val bSig = b.ts map sigma

      //no need to bind the synthesized types, since in this case, they exactly match b.ts
      val (_, infEs) = ((b.ts zip es) map { case (p, e) => infer(p, gamma, tgamma, e)}).unzip

      (sigma(b.res) matchUp proto, BaseCall(b, ts, infEs))

    case _ => throw InferenceException(s"Cannot infer type for $exp. Unknown syntactic form.")
  }

  def inferRule(gamma: Context, tgamma: TVarContext, r: Rule, srvSignature: TSrvRep): (Type, Rule) = {
    val ruleGamma = gamma ++ r.rcvars + ('this -> TSrv(srvSignature))
    val (t, body) = infer(Unit, ruleGamma, tgamma, r.p)
    (t, Rule(r.ps, body))
  }

  def inferRule(gamma: Context, tgamma: TVarContext, r: URule, srvSignature: TSrvRep): (Type, Rule) = {
    val typedps = r.ps map { p =>
      srvSignature.svcs(p.name) match {
        case TSvc(ts) if ts.length == p.params.length =>
          val args = ListMap((p.params zip ts):_*)
          Pattern(p.name, args)
      }
    }
    val rcvars = Rule(typedps, Par()).rcvars

    val ruleGamma = gamma ++ rcvars + ('this -> TSrv(srvSignature))
    val (t, body) = infer(Unit, ruleGamma, tgamma, r.p)
    (t, Rule(typedps, body))
  }

  case class InferenceException(msg: String) extends RuntimeException(msg)
}

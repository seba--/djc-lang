package djc.lang.typ.inference

import djc.lang.Gensym
import djc.lang.{TypedLanguage => TS}
import djc.lang.typ._
import djc.lang.typ.inference.{ExtLanguage => XS, ProtoTypes => PT}
import util.Bag

import scala.collection.immutable.ListMap

import Constraints._
import Matching._
import XS.{op => _, _}
import Checker._

import TS.types._
import TS.types.op._

object TypeInference {
  type XExp = XS.Exp
  type Exp = TS.Exp
  type PType = PT.Type
  type Type = TS.types.Type

  implicit def asPrototype[TF <: TypeFamily](t: TF#Type): PT.Type = t.toFamily(PT)

  def infer(proto: PType, gamma: Context, tgamma: TVarContext, exp: XExp): (Type, Exp) = {
    implicit val ctx = (proto, gamma, tgamma, exp)
    implicit val tg: TVarContext = tgamma
    try {
      exp match {
        case Var(x) =>
          (gamma(x) matchUp proto, exp.toFamily(TS))

        case Par(ps) => proto match {
          case PT.Hole | PT.Unit | PT.Top =>
            val (psts, pses) = ps.map(infer(PT.Unit, gamma, tgamma, _)).unzip
            val joined = psts.foldLeft(Unit.asInstanceOf[Type]) { (t, u) => join(tgamma)(t, u)}
            val tres = if (proto == PT.Hole) PT.Unit else proto

            (joined matchUp tres, TS.Par(pses))
        }

        case ExtSyntaxDerived.Let(x, e1, body) =>
          val letSymbol = ExtSyntaxDerived.Let.letSymbol
          val (tpe, ie1) = infer(PT.Hole, gamma, tgamma, e1)
          val (tpe2, ibody) = infer(PT.Hole, gamma + (x -> tpe), tgamma, body)

          (Unit, TS.Send(TS.ServiceRef(TS.LocalServer(TS.Rule(Bag(TS.Pattern(letSymbol, x -> tpe)), ibody)), letSymbol), List(ie1)))

        case ExtSyntaxDerived.This(usrv, self) =>
          val letSymbol = ExtSyntaxDerived.This.letSymbol
          val (thisType, iself) = infer(PT.Hole, gamma, tgamma, self)
          val (_, srv@TS.ServerImpl(_)) = infer(PT.TSrvRep(letSymbol -> PT.TSvc(thisType.toFamily(PT))), gamma, tgamma, usrv)

          (Unit, TS.Send(TS.ServiceRef(TS.SpawnLocal(srv), letSymbol), List(iself)))

        case Send(rcv, args) => proto match {
          case PT.Hole | PT.Unit | PT.Top =>
            val (t, rcve) = infer(PT.Hole, gamma, tgamma, rcv)
            val TSvc(ts) = promote(tgamma)(t)
            if (ts.length != args.length)
              throw InferenceException(s"receiver/arg counts do not match: inferred $t for rcv\n with arguments $args")
            val (_, arges) = (ts zip args).map { case (t, e) => infer(t, gamma, tgamma, e)}.unzip
            val tres = if (proto == PT.Hole) Unit else proto.toFamily(TS.types)

            (tres, TS.Send(rcve, arges))

          case p =>
            val (Bot, rcve) = infer(PT.Hole, gamma, tgamma, rcv)
            val (_, arges) = args.map(infer(PT.Top, gamma, tgamma, _)).unzip

            (Bot matchUp p, TS.Send(rcve, arges))
        }

        case ServiceRef(srv, x) =>
          val (t, srve) = infer(PT.TSrv(PT.TSrvRep(x -> proto)), gamma, tgamma, srv)
          val TSrv(TSrvRep(svcs)) = promote(tgamma)(t)
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
            if (PT.op.isPrototype(proto))
              throw InferenceException(s"Type of server must be fully known from the usage context, got incomplete type $proto\n\n$srv")
            if (!(psvcs.keySet subsetOf srv.services.keySet))
              throw InferenceException(s"Untyped server $srv is incompatible with prototype $proto")

            val tsrv@TSrvRep(svcs) = proto.toFamily(TS.types)

            //assume services not mentioned in proto to be of the most general service type
            val added = for ((svc, arity) <- srv.services if !svcs.contains(svc))
            yield svc -> TSvc(List.fill(arity)(Bot))
            val tsrv2 = TSrvRep(svcs ++ added)

            val (_, irules) = rules.map(inferRule(gamma, tgamma, _, tsrv2)).unzip
            (tsrv, TS.ServerImpl(irules))
        }

        case Spawn(local, e) =>
          proto match {
            case PT.Hole | PT.Top =>
              val (tpe, ie) = infer(PT.Hole, gamma, tgamma, e)
              val tsrv@TSrvRep(_) = promote(tgamma)(tpe)
              (TSrv(tpe) matchUp proto, TS.Spawn(local, ie))

            case PT.TSrv(p@PT.TSrvRep(_)) =>
              val (t, ie) = infer(p, gamma, tgamma, e)
              (TSrv(t), TS.Spawn(local, ie))

            case p =>
              val (t, ie) = infer(PT.Hole, gamma, tgamma, e)
              if (promote(tgamma)(t) == Bot)
                (t matchUp p, TS.Spawn(local, ie))
              else throw InferenceException(s"Cannot infer argument type of spawn: $e with proto $p")
          }

        //TODO Bot case
        case TApp(e, t) =>
          val (it, ie) = infer(PT.Hole, gamma, tgamma, e)
          val TUniv(alpha, bound, t2) = promote(tgamma)(it)
          if (!subtype(tgamma)(t, bound))
            throw InferenceException(s"Type application of $t does not satisfy inferred bound $bound")
          (substType(alpha -> t)(t2), TS.TApp(ie, t))

        //TODO Bot case
        case UTApp(e) =>
          val (tt, ie) = infer(PT.Hole, gamma, tgamma, e)
          val TUniv(alpha, bound, t) = promote(tgamma)(tt)
          val c1 = GenConstraints(tgamma, alpha, t, Top matchDown proto)
          val c2 = GenConstraints(tgamma, alpha, TVar(alpha), bound)
          val subst = solve(tgamma, alpha, Constraints.meet(tgamma, c1, c2), t)
          val sigma = op.substType(alpha -> subst)
          val tres = sigma(t)
          (tres matchUp proto, TS.TApp(ie, subst))

        case TAbs(alpha, bound, e) =>
          proto match {
            case PT.Hole | PT.Top =>
              val (t, ie) = infer(proto, gamma, tgamma + (alpha -> bound), e) //TODO have to check if alpha is already in scope
              (TUniv(alpha, bound, t), TS.TAbs(alpha, bound, ie))

            case PT.TUniv(alpha2, bound2, p) if !PT.op.isPrototype(bound2) && bound === bound2.toFamily(TS.types) =>
              lazy val fptv = PT.op.freeTypeVars(p) //TODO have to check if alpha is already in scope
            val alphares =
              if (alpha != alpha2 && fptv(alpha))
                Gensym(alpha, XS.op.freeTypeVars(e) ++ fptv)
              else alpha

              val (te, ie) = infer(PT.op.substType(alpha2 -> PT.TVar(alphares))(p),
                gamma, tgamma + (alphares -> bound),
                XS.op.substType(alpha -> TVar(alphares))(e))
              (TUniv(alphares, bound, te), TS.TAbs(alphares, bound, ie))
          }

        case UnsafeCast(e, t) =>
          val (_, ie) = infer(PT.Hole, gamma, tgamma, e)
          (t matchUp proto, TS.UnsafeCast(ie, t))

        case UpCast(e, t) =>
          val (_, ie) = infer(t, gamma, tgamma, e)
          (t matchUp proto, TS.UpCast(ie, t))

        case BaseCall(b, Nil, es) =>
          //infer type arguments
          val ((tvarslist0, bounds), ts0, tres0) = (b.targs.unzip, b.ts, b.res)
          val conflicts = tvarslist0.filter(tgamma.keySet)
          val rename = conflicts zip Gensym(conflicts, tgamma.keySet)
          val renamef: Symbol => Symbol = Map[Symbol, Symbol](rename: _*).orElse { case x => x}
          val sigmar = substType(rename.map { case (a, b) => (a, TVar(b))})
          val (tvarslist, ts, tres) = (tvarslist0.map(renamef), ts0.map(sigmar(_)), sigmar(tres0))
          val tvars = tvarslist.toSet
          val substTargs = PT.op.substType(tvarslist zip List.fill(tvarslist.length)(PT.Hole))
          val (argTypes, inferredArgs) = ((ts zip es) map { case (t, e) => infer(substTargs(t), gamma, tgamma, e)}).unzip

          val cargs = GenConstraints(tgamma, tvars, argTypes zip ts)
          val cresult = GenConstraints(tgamma, tvars, tres, Top matchDown proto)
          //TODO forbid interdependent bounds
          val cbounds = GenConstraints(tgamma, tvars, tvarslist.map(TVar(_)) zip bounds)
          val call = Constraints.meet(tgamma, Seq(cargs, cresult, cbounds))
          val sigma = substType(solve(tgamma, tvars, call, tres))

          (sigma(tres) matchUp proto, TS.BaseCall(b.toFamily(TS), tvarslist map (s => sigma(TVar(s))), inferredArgs))

        case BaseCall(b, ts, es) => //TODO rename type variables when necessary
          if (b.targs.length != ts.length)
            throw InferenceException(s"Insufficient type arguments ($ts) provided for baseop $b.")

          val (tArgs, bounds) = b.targs.unzip
          if (!ts.corresponds(bounds)(subtype(tgamma)(_, _)))
            throw InferenceException(s"Type arguments do not match bounds of type parameters. Applied $ts to ${b.targs}\n in $exp")

          val sigma: Type => Type = substType(tArgs zip ts)(_)
          val bSig = b.ts map sigma

          //no need to bind the synthesized types, since in this case, they exactly match b.ts
          val (_, infEs) = ((b.ts zip es) map { case (p, e) => infer(p, gamma, tgamma, e)}).unzip
          (sigma(b.res) matchUp proto, TS.BaseCall(b.toFamily(TS), ts, infEs))

        case _ => throw InferenceException(s"Cannot infer type for $exp. Unknown syntactic form.")
      }
    }
    catch {
      case i: InferenceException => throw i
      case e: RuntimeException =>
        throw InferenceException("type inference failed", proto, gamma, tgamma, exp, e)
    }
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

  case class InferenceException(msg: String, proto: PType, gamma: Context, tgamma: TVarContext, exp: XExp, var cause: RuntimeException) extends RuntimeException(msg)
  object InferenceException {
    def apply(msg: String, cause: RuntimeException = null)(implicit ctx: (PType, Context, TVarContext, XExp)): InferenceException = InferenceException(msg, ctx._1, ctx._2, ctx._3, ctx._4, cause)
  }
}

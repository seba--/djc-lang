package djc.lang.sem.nondeterm_routed

import scala.Symbol
import djc.lang.Mapper._
import scala.language.postfixOps
import util.Bag
import djc.lang.sem.{Substitution, Crossproduct, AbstractSemantics}


import djc.lang._
import djc.lang.Def
import djc.lang.ServerImpl
import djc.lang.Send
import djc.lang.ServerVar
import djc.lang.ServiceRef
import djc.lang.Rule
import djc.lang.Pattern
import djc.lang.Par

import Substitution._
import Crossproduct._
import Data._


object Semantics extends AbstractSemantics[Bag[SendClosure]] {

  def normalizeVal(v: Val) = v map (_.normalize)

  override def interp(p: Prog) = {
    Router.routeTable = collection.mutable.Map()
    interp(p, Map())
  }

  def interp(p: Prog, env: Env): Res[Val] = p match {
    case Def(x, s@ServerVar(y), p) if env.isDefinedAt(y) => {
      interp(p, env + (x -> env(y)))
    }
    case Def(x, s@ServerImpl(_), p) => {
      val addr = Router.registerServer(ServerClosure(s, env))
      interp(p, env + (x -> ServerAddr(addr)))
    }
    case Par(ps) => {
      nondeterministic(
        crossProduct(ps map (interp(_, env))),
        (x: Val) => interpSends(x))
    }
    case s@Send(rcv, args) => interpSends(Bag(SendClosure(s, env)))
    case cl@SendClosure(s, env) => Set(Bag(cl))
  }

  def interpSends(v: Val): Res[Val] = {
    val canSend = selectSends(v)
    if (canSend.isEmpty)
      Set(v)
    else
      nondeterministic(
        canSend,
        (p: (RuleClosure, Match)) => {
          val cl = p._1
          val ma = p._2
          val (prog, restSends) = fireRule(cl, ma, v)
          val ienv = cl.env + ('this -> cl.server)
          interp(Par(Bag(prog) ++ restSends), ienv) // env has no effect on `restSends`, but is needed for `p`
        })
  }

  def selectSends(v: Val): Res[(RuleClosure, Match)] =
    nondeterministic(
      (v map (collectRules(_))).flatten,
      (p: (Server, RuleClosure)) =>
        matchRule(p._1, p._2.rule.ps, v) map (x => (p._2, x))
    )

  def matchRule(server: Server, pats: Bag[Pattern], v: Val): Res[Match] =
    if (pats.isEmpty)
      Set(Match(Map(), Bag()))
    else {
      val name = pats.head.name
      val params = pats.head.params
      val matchingSends = v filter({
        case SendClosure(Send(ServiceRef(`server`, `name`), args), env) => params.size == args.size
        case _ => false
      })
      nondeterministic(
        matchingSends,
        (cl: SendClosure) => matchRule(server, pats.tail, v - cl) map (
          p => Match(p.subst ++ (params zip cl.send.args), p.used + cl)
        )
      )
    }

  def fireRule(cl: RuleClosure, ma: Match, orig: Val): (Prog, Val) = {
    var p = cl.rule.p // map(substServer('this, server), rule.p)
    for ((x, s) <- ma.subst)
      p = map(substService(x, s), p)
    val restSends = orig diff ma.used
    (p, restSends)
  }

  def collectRules(cl: SendClosure): Bag[(Server, RuleClosure)] = cl match {
    case SendClosure(Send(ServiceRef(sa@ServerAddr(_), _), _), env) => {
      val sc = lookupAddr(sa)
      sc.ths.rules map ((r: Rule) => (sa, RuleClosure(r, sa, sc.env)))
    }
    case SendClosure(Send(ServiceRef(sv@ServerVar(x), y), args), env) if env.isDefinedAt(x) => {
      val sc = lookupAddr(env(x))
      for ((v,r) <- collectRules(SendClosure(Send(ServiceRef(sc.ths, y), args), sc.env)))
        yield (sv, r)
    }
  }
}
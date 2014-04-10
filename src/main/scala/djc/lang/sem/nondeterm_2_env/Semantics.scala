package djc.lang.sem.nondeterm_2_env

import djc.lang.sem.{Crossproduct, AbstractSemantics}
import djc.lang.sem.Substitution._
import util.Bag
import djc.lang.Syntax._
import djc.lang.Syntax.Mapper._
import Crossproduct._
import Data._

object Semantics extends AbstractSemantics[Bag[SendClosure]] {

  def normalizeVal(v: Val) = v map (_.normalize)

  override def interp(p: Prog) = interp(p, Map())

  def interp(p: Prog, env: Env): Res[Val] = p match {
    case Def(x, s@ServerVar(y), p) if env.isDefinedAt(y) =>
      interp(p, env + (x -> env(y)))
    case Def(x, s@ServerImpl(_), p) =>
      interp(p, env + (x -> ServerClosure(s, env)))
    case Par(ps) => {
      nondeterministic(
        crossProduct(ps map (interp(_, env))),
        (x: Val) => interpSends(x))
    }
    case s@Send(rcv, args) => interpSends(Bag(SendClosure(s, env)))
    case sc@SendClosure(_, _) => Set(Bag(sc))
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
          val ienv = cl.env + ('this -> ServerClosure(cl.server, cl.env))
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
      val matchingSends = v.filter({
        case SendClosure(Send(ServiceRef(`server`, `name`), args), _) => params.size == args.size
        case _ => false
      })
      nondeterministic(
        matchingSends,
        (s: SendClosure) => matchRule(server, pats.tail, v - s) map (
          p => Match(p.subst ++ (params zip s.send.args), p.used + s)
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

  def collectRules(v: SendClosure): Bag[(Server, RuleClosure)] = v match {
    case SendClosure(Send(ServiceRef(sv@ServerVar(x), y), args), env) if env.isDefinedAt(x) => {
      val sc = env(x)
      for ((v,r) <- collectRules(SendClosure(Send(ServiceRef(sc.ths, y), args), sc.env)))
        yield (sv, r)
    }
    case SendClosure(Send(ServiceRef(si@ServerImpl(rules), _), _), env) =>
      rules map ((r: Rule) => (si, RuleClosure(r, si, env)))
  }
}

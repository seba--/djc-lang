package djc.lang.sem.nondeterm_4_grouped

import scala.Symbol
import scala.language.postfixOps
import util.Bag
import djc.lang.Mapper._
import djc.lang.sem.{Substitution, Crossproduct, AbstractSemantics}
import djc.lang._
import djc.lang.Def
import djc.lang.ServerImpl
import djc.lang.Send
import djc.lang.ServerVar
import scala.Some
import djc.lang.ServiceRef
import djc.lang.Rule
import djc.lang.Pattern
import djc.lang.Par


object Semantics extends AbstractSemantics[Data.Servers] {
  import Substitution._
  import Crossproduct._
  import Data._

  def normalizeVal(v: Val) = (Bag() ++ v.values).flatten map (_.normalize)

  override def interp(p: Prog) = {
    Router.routeTable = collection.mutable.Map()
    interp(p, Map(), Map())
  }

  def interp(p: Prog, env: Env, servers: Servers): Res[Val] = p match {
    case Def(x, s@ServerVar(y), p) if env.isDefinedAt(y) => {
      interp(p, env + (x -> env(y)), servers)
    }
    case Def(x, s@ServerImpl(_), p) => {
      val addr = Router.registerServer(ServerClosure(s, env))
      interp(p, env + (x -> ServerAddr(addr)), servers + (addr -> Bag()))
    }
    case Par(ps) => {
      nondeterministic(
        crossProductMap(ps map (interp(_, env, servers))),
        (x: Val) => interpSends(x))
    }
    case s@Send(ServiceRef(ServerVar(x), _), args) if env.isDefinedAt(x) => {
      val addr = ServerAddr.unapply(env(x)).get
      interpSends(sendToServer(servers, addr, SendClosure(s, env)))
    }
  }

  def interpSends(v: Val): Res[Val] = {
    val canSend = selectServerSends(v)
    if (canSend.isEmpty)
      Set(v)
    else
      nondeterministic(
        canSend,
        (p: (RuleClosure, Match)) => {
          val cl = p._1
          val ma = p._2
          val (prog, env, newServers) = fireRule(cl, ma, v)
          interp(prog, env, newServers) // env has no effect on `restSends`, but is needed for `p`
        })
  }

  def selectServerSends(servers: Servers): Res[(RuleClosure, Match)] =
    servers.values.toSet.map((bag: Bag[SendClosure]) => selectSends(bag)).flatten

  def selectSends(v: Bag[SendClosure]): Res[(RuleClosure, Match)] =
    nondeterministic(
      (v map (collectRules(_))).flatten,
      (r: (Server, RuleClosure)) =>
        matchRule(r._1, r._2.rule.ps, v) map (x => (r._2, x))
    )

  def matchRule(server: Server, pats: Bag[Pattern], v: Bag[SendClosure]): Res[Match] =
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

  def fireRule(cl: RuleClosure, ma: Match, orig: Val): (Prog, Env, Val) = {
    var p = cl.rule.p
    for ((x, s) <- ma.subst)
      p = map(substService(x, s), p)

    val addr = ServerAddr.unapply(cl.server).get
    val oldQueue = orig(addr)
    val newQueue = oldQueue diff ma.used
    val newServers = orig.updated(addr, newQueue)

    val env = cl.env + ('this -> cl.server)
    (p, env, newServers)
  }

  def collectRules(cl: SendClosure): Bag[(Server, RuleClosure)] = cl match {
    case SendClosure(Send(ServiceRef(sa@ServerAddr(_), _), _), env) => {
      val sc = lookupAddr(sa)
      sc.ths.rules map ((r: Rule) => (sa, RuleClosure(r, sa, sc.env)))
    }
    case SendClosure(Send(ServiceRef(sv@ServerVar(x), y), args), env) if env.isDefinedAt(x) => {
      val saddr = env(x)
      for ((v,r) <- collectRules(SendClosure(Send(ServiceRef(saddr, y), args), env)))
        yield (sv, r)
    }
  }
}

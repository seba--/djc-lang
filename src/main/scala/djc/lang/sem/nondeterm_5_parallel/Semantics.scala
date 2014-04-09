package djc.lang.sem.nondeterm_5_parallel

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
    case ProgClosure(p, env) => interp(p, env, servers)
  }

  def interpSends(v: Val): Res[Val] = {
    val canSend = selectServerSends(v)
    if (canSend.isEmpty)
      Set(v)
    else
      nondeterministic(
        canSend,
        (p: Bag[(RuleClosure, Match)]) => {
          val (newProgs, newServers) = fireRules(p, v)
          val progClosures = newProgs map (p => ProgClosure(p._1, p._2))
          interp(Par(Bag() ++ progClosures), Map(), newServers)
        })
  }

  def selectServerSends(servers: Servers): Res[Bag[(RuleClosure, Match)]] = {
    val bag = (Bag() ++ servers.values).map(selectSends(_)).filter(!_.isEmpty)
    if (bag.isEmpty)
      Set()
    else
      crossProductAlt(bag)
  }


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

  def fireRules(rules: Bag[(RuleClosure, Match)], oldServers: Servers): (Bag[(Prog, Env)], Servers) = {
    var newServers = oldServers
    val newProgs = rules map (p => { // fire rules in parallel
      val addr = ServerAddr.unapply(p._1.server).get
      val oldQueue = oldServers(addr)
      val (prog, env, newQueue) = fireRule(p._1, p._2, oldQueue)
      newServers = newServers updated(addr, newQueue)
      (prog, env)
    })

    (Bag() ++ newProgs, newServers)
  }

  def fireRule(cl: RuleClosure, ma: Match, oldQueue: Bag[SendClosure]): (Prog, Env, Bag[SendClosure]) = {
    var p = cl.rule.p
    for ((x, s) <- ma.subst)
      p = map(substService(x, s), p)

    val addr = ServerAddr.unapply(cl.server).get
    val newQueue = oldQueue diff ma.used
//    val newServers = orig.updated(addr, newQueue)

    val env = cl.env + ('this -> cl.server)
    (p, env, newQueue)
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

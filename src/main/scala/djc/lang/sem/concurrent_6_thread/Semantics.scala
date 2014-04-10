package djc.lang.sem.concurrent_6_thread

import util.Bag

import djc.lang.Syntax._
import djc.lang.sem.AbstractSemantics

import Data._
import djc.lang.Syntax.Mapper._
import djc.lang.sem.Substitution._
import djc.lang.sem.Crossproduct._

/**
 * Created by seba on 09/04/14.
 */
object Semantics extends AbstractSemantics[Unit] { // all data is in the global state
  def normalizeVal(v: Val) = ((Bag() ++ Router.routeTable.values) map (_.normalizeVal)).flatten

  override def interp(p: Prog): Res[Val] = {
    Router.routeTable = collection.mutable.Map()
    val res = interp(p, Map())
    Thread.sleep(50)
    Router.routeTable.values.map(_.terminate = true)
    res
  }

  def interp(p: Prog, env: Env): Res[Val] = p match {
    case Def(x, s@ServerVar(y), p) if env.isDefinedAt(y) =>
      interp(p, env + (x -> env(y)))
    case Def(x, impl@ServerImpl(_), p)
    => {
      val server = new ServerThread(impl, env)
      val addr = Router.registerServer(server)
      server.addr = ServerAddr(addr)
      server.start()
      interp(p, env + (x -> ServerAddr(addr)))
    }
    case Par(ps) => {
      ps map (interp(_, env))
      Set(Unit)
    }
    case s@Send(ServiceRef(ServerVar(x), _), args) if env.isDefinedAt(x) => {
      lookupAddr(env(x)).sendRequest(SendClosure(s, env))
      Set(Unit)
    }
    case ProgClosure(p, env) => interp(p, env)
  }

  def interpSends(server: ServerThread) {
    for (r <- server.impl.rules) {
      val canSend = matchRule(r.ps, server.inbox)
      if (!canSend.isEmpty) {
        val ma = canSend.head
        val (newProg, env, newQueue) = fireRule(RuleClosure(r, server.addr, server.env), ma, server.inbox)
        server.inbox = newQueue
        interp(newProg, env)
        return
      }
    }
  }

  def matchRule(pats: Bag[Pattern], v: Bag[SendClosure]): Res[Match] =
    if (pats.isEmpty)
      Set(Match(Map(), Bag()))
    else {
      val name = pats.head.name
      val params = pats.head.params
      val matchingSends = v filter({
        case SendClosure(Send(ServiceRef(s, `name`), args), env) if params.size == args.size => true
        case _ => false
      })
      nondeterministic(
        matchingSends,
        (cl: SendClosure) => matchRule(pats.tail, v - cl) map (
          p => Match(p.subst ++ (params zip cl.send.args), p.used + cl)
          )
      )
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

}

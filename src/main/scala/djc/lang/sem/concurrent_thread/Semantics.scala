package djc.lang.sem.concurrent_thread

import util.Bag

import djc.lang._
import djc.lang.sem.AbstractSemantics

import Data._
import djc.lang.Mapper._
import djc.lang.sem.Substitution._
import djc.lang.sem.Crossproduct._

/**
 * Created by seba on 09/04/14.
 */
object Semantics extends AbstractSemantics[Unit] { // all data is in the global state
  def normalizeVal(v: Val) = ((Bag() ++ Router.routeTable.values) map (_.normalizeVal)).flatten

  override def interp(p: Prog): Res[Val] = {
    Router.routeTable = collection.mutable.Map()
    interp(p, Map())
    Thread.sleep(50)
    Router.routeTable.values.map(_.terminate = true)
    Set(Unit)
  }

  def interp(p: Prog, envServer: Env): Res[Val] = p match {
    case Def(x, addr@ServerAddr(_), p)
    => interp(p, envServer + (x -> addr))
    case Def(x, ServerVar(y), p)
    => envServer get(y) match {
      case Some(s) => interp(p, envServer + (y -> s))
      case None => throw new IllegalArgumentException(s"Unbound server variable $y in ${Def(x, ServerVar(y), p)}")
    }
    case Def(x, impl@ServerImpl(_), p)
    => {
      val server = new ServerThread(impl, envServer)
      val addr = Router.registerServer(server)
      server.addr = ServerAddr(addr)
      server.start()
      interp(p, envServer + (x -> ServerAddr(addr)))
    }
    case Par(ps) => {
      ps map (interp(_, envServer))
      Set(Unit)
    }
    case s@Send(ServiceRef(ServerAddr(addr), _), args) => {
      Router.lookupAddr(addr).sendRequest(Closure(s, envServer))
      Set(Unit)
    }
    case s@Send(ServiceRef(ServerVar(x), _), args) if envServer.isDefinedAt(x) => {
      lookupAddr(envServer(x)).sendRequest(Closure(s, envServer))
      Set(Unit)
    }
    case ClosureProg(p, env) => interp(p, env)
  }

  def matchRule(pats: Bag[Pattern], v: Bag[Closure]): Res[(Map[Symbol, Service], Bag[Closure])] =
    if (pats.isEmpty)
      Set((Map(), Bag()))
    else {
      val name = pats.head.name
      val params = pats.head.params
      val matchingSends = v filter({
        case Closure(Send(ServiceRef(s, `name`), args), env) if params.size == args.size => true
        case _ => false
      })
      nondeterministic(
        matchingSends,
        (cl: Closure) => matchRule(pats.tail, v - cl) map (
          p => (p._1 ++ (params zip cl.send.args), p._2 + cl)
          )
      )
    }

  def fireRule(server: ServerAddr, rule: Rule, subst: Map[Symbol, Service], used: Bag[Closure], oldQueue: Bag[Closure]): (Prog, Bag[Closure]) = {
    var p = map(substServer('this, server), rule.p)
    for ((x, s) <- subst)
      p = map(substService(x, s), p)

    val newQueue = oldQueue diff used
    (p, newQueue)
  }
}

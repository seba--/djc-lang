package djc.lang.sem.nondeterm_parallel

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

  def interp(p: Prog, envServer: EnvServer, servers: Servers): Res[Val] = p match {
    case Def(x, addr@ServerAddr(_), p)
    => interp(p, envServer + (x -> addr), servers)
    case Def(x, ServerVar(y), p)
    => envServer get(y) match {
      case Some(s) => interp(p, envServer + (y -> s), servers)
      case None => throw new IllegalArgumentException(s"Unbound server variable $y in ${Def(x, ServerVar(y), p)}")
    }
    case Def(x, s@ServerImpl(_), p)
    => {
      val addr = Router.registerServer(s)
      interp(p, envServer + (x -> ServerAddr(addr)), servers + (addr -> Bag()))
    }
    case Par(ps) => {
      nondeterministic(
        crossProductMap(ps map (interp(_, envServer, servers))),
        (x: Val) => interpSends(x))
    }
    case s@Send(ServiceRef(ServerAddr(addr), _), args) => {
      interpSends(sendToServer(servers, addr, Closure(s, envServer)))
    }
    case s@Send(ServiceRef(ServerVar(x), _), args) if envServer.isDefinedAt(x) => {
      val addr = ServerAddr.unapply(envServer(x)).get
      interpSends(sendToServer(servers, addr, Closure(s, envServer)))
    }
    case ClosureProg(p, env) => interp(p, env, servers)
  }

  def interpSends(v: Val): Res[Val] = {
    val canSend = selectServerSends(v)
    if (canSend.isEmpty)
      Set(v)
    else
      nondeterministic(
        canSend,
        (p: Bag[(ServerAddr, Rule, EnvServer, Map[Symbol, Service], Bag[Closure])])
           => fireRules(p, v))
  }

  def selectServerSends(servers: Servers): Res[Bag[(ServerAddr, Rule, EnvServer, Map[Symbol, Service], Bag[Closure])]] = {
    type R = Res[(ServerAddr, Rule, EnvServer, Map[Symbol, Service], Bag[Closure])]
    val bag = (Bag() ++ servers.values).map((b: Bag[Closure]) => selectSends(b)).filter(!_.isEmpty)
    if (bag.isEmpty)
      Set()
    else
      crossProductAlt(bag)
  }


  def selectSends(v: Bag[Closure]): Res[(ServerAddr, Rule, EnvServer, Map[Symbol, Service], Bag[Closure])] =
    nondeterministic(
      (v map (collectRules(_))).flatten,
      (r: (ServerAddr, Rule, EnvServer)) =>
        matchRule(r._1, r._2.ps, v) map (x => (r._1, r._2, r._3, x._1, x._2))
    )

  def matchRule(server: ServerAddr, pats: Bag[Pattern], v: Bag[Closure]): Res[(Map[Symbol, Service], Bag[Closure])] =
    if (pats.isEmpty)
      Set((Map(), Bag()))
    else {
      val name = pats.head.name
      val params = pats.head.params
      val matchingSends = v filter({
        case Closure(Send(ServiceRef(s, `name`), args), env)
          if params.size == args.size && serverEqual(server, s, env) => true
        case _ => false
      })
      nondeterministic(
        matchingSends,
        (cl: Closure) => matchRule(server, pats.tail, v - cl) map (
          p => (p._1 ++ (params zip cl.send.args), p._2 + cl)
          )
      )
    }

  def serverEqual(s1: Server, s2: Server, env: EnvServer): Boolean = (s1, s2) match {
    case (ServerAddr(addr1), ServerAddr(addr2)) if addr1 == addr2 => true
    case (ServerVar(x1), ServerVar(x2)) if x1 == x2 => true
    case (ServerVar(x1), s2) if env.isDefinedAt(x1) => serverEqual(env(x1), s2, env)
    case (s1, ServerVar(x2)) if env.isDefinedAt(x2) => serverEqual(s1, env(x2), env)
    case (s1@ServerImpl(_), s2@ServerAddr(_)) => serverEqual(s1, lookupAddr(s2), env)
    case (s1@ServerAddr(_), s2@ServerImpl(_)) => serverEqual(lookupAddr(s1), s2, env)
    case (s1@ServerImpl(_), s2@ServerImpl(_)) => s1 == s2
  }

  def fireRules(rules: Bag[(ServerAddr, Rule, EnvServer, Map[Symbol, Service], Bag[Closure])], oldServers: Servers): Res[Val] = {
    val updates = rules map (p => { // fire rule in parallel
      val addr = ServerAddr.unapply(p._1).get
      val oldQueue = oldServers(addr)
      val (prog, newQueue) = fireRule(p._1, p._2, p._3, p._4, p._5, oldQueue)
      (ClosureProg(prog, p._3), addr, newQueue)
    })

    val newProgs = updates map (_._1)

    val newServers = updates.foldLeft(oldServers)((ss: Servers, u: (Prog, Router.Addr, Bag[Closure])) => ss + (u._2 -> u._3))

    interp(Par(Bag() ++ newProgs), Map(), newServers)
  }

  def fireRule(server: ServerAddr, rule: Rule, env: EnvServer, subst: Map[Symbol, Service], used: Bag[Closure], oldQueue: Bag[Closure]): (Prog, Bag[Closure]) = {
    var p = map(substServer('this, server), rule.p)
    for ((x, s) <- subst)
      p = map(substService(x, s), p)

    val newQueue = oldQueue diff used
    (p, newQueue)
  }

  def collectRules(cl: Closure): Bag[(ServerAddr, Rule, EnvServer)] = cl match {
    case Closure(Send(ServiceRef(addr@ServerAddr(_), _), _), env) => {
      val rules = lookupAddr(addr).rules
      rules map ((addr, _, env))
    }
    case Closure(Send(ServiceRef(ServerVar(x), _), _), env) if env.isDefinedAt(x) => {
      val addr = env(x)
      val rules = lookupAddr(addr).rules
      rules map ((addr, _, env))
    }
    case Closure(Send(_, _), _) => Bag()
  }
}

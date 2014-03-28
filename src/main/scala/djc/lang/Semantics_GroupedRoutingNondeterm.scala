package djc.lang

import scala.Symbol
import scala.language.postfixOps
import util.Bag
import djc.lang.Mapper._
import scala.Some

object Semantics_GroupedRoutingNondeterm_Router {
  type Addr = String

  var routeTable: collection.mutable.Map[Addr, ServerImpl] = null

  var addrNum = 0
  val addrPrefix = "Server@"
  def nextAddr: Addr = {
    addrNum += 1
    val addr = addrPrefix + addrNum
    if (!routeTable.isDefinedAt(addr))
      addr
    else
      nextAddr
  }

  def registerServer(s: ServerImpl): Addr = {
    val addr = nextAddr
    routeTable += (addr -> s)
    addr
  }

  def lookupAddr(addr: Addr): ServerImpl = routeTable(addr)
}

object Semantics_GroupedRoutingNondeterm_Data {
  import Substitution._
  import djc.lang.{Semantics_GroupedRoutingNondeterm_Router => Router}

  type ServerAddr = ServerVar
  object ServerAddr {
    val prefix = "ADDR:"
    def apply(addr: Router.Addr) = new ServerAddr(Symbol(prefix + addr))
    def unapply(s: ServerVar): Option[Router.Addr] = getAddr(s.x.name)

    def getAddr(name: String): Option[Router.Addr] =
      if (name.startsWith(prefix))
        Some(name.substring(prefix.length))
      else
        None
  }
  def lookupAddr(a: ServerAddr): ServerImpl = a match {
    case ServerAddr(addr) => Router.lookupAddr(addr)
    case _ => throw new IllegalArgumentException(s"Not a server address: $a")
  }

  type EnvServer = Map[Symbol, ServerAddr]
  case class Closure(send: Send, env: EnvServer) extends Prog {
    def normalize = env.toList.reverse.foldLeft(send)((s: Send, p: (Symbol, ServerAddr)) => map(substServer(p._1,lookupAddr(p._2)), s).asInstanceOf[Send])
  }

  type Servers = Map[Router.Addr, Bag[Closure]]
  def sendToServer(servers: Servers, addr: Router.Addr, cl: Closure) =
    servers + (addr -> (servers(addr) + cl))
}

object Semantics_GroupedRoutingNondeterm extends AbstractSemantics[Semantics_GroupedRoutingNondeterm_Data.Servers] {
  import Substitution._
  import Crossproduct._
  import Semantics_GroupedRoutingNondeterm_Data._
  import djc.lang.{Semantics_GroupedRoutingNondeterm_Router => Router}

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
  }

  def interpSends(v: Val): Res[Val] = {
    val canSend = selectServerSends(v)
    if (canSend.isEmpty)
      Set(v)
    else
      nondeterministic(
        canSend,
        (p: (ServerAddr, Rule, EnvServer, Map[Symbol, Service], Bag[Closure])) => fireRule(p._1, p._2, p._3, p._4, p._5, v))
  }

  def selectServerSends(servers: Servers): Res[(ServerAddr, Rule, EnvServer, Map[Symbol, Service], Bag[Closure])] =
    servers.values.toSet.map((bag: Bag[Closure]) => selectSends(bag)).flatten

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

  def fireRule(server: ServerAddr, rule: Rule, env: EnvServer, subst: Map[Symbol, Service], used: Bag[Closure], oldServers: Servers): Res[Val] = {
    var p = map(substServer('this, server), rule.p)
    for ((x, s) <- subst)
      p = map(substService(x, s), p)
    
    val addr = ServerAddr.unapply(server).get
    val oldQueue = oldServers(addr)
    val newQueue = oldQueue diff used
    val newServers = oldServers.updated(addr, newQueue)
    
    interp(Par(Bag(p)), env, newServers)
  }

  def collectRules(cl: Closure): Bag[(ServerAddr, Rule, EnvServer)] = cl match {
    case Closure(Send(ServiceRef(s@ServerImpl(rules), _), _), env) => {
      throw new IllegalStateException(s"Found send to unregistered server $cl")
      //      val addr = Router.registerServer(s)
      //      rules map ((ServerAddr(addr), _, env))
    }
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

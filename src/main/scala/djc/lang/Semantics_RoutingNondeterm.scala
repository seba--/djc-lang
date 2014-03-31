package djc.lang

import scala.Symbol
import djc.lang.Folder._
import djc.lang.Mapper._
import scala.language.postfixOps
import util.Bag

object Semantics_RoutingNondeterm_Router {
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

object Semantics_RoutingNondeterm_Data {
  import Substitution._
  import djc.lang.{Semantics_RoutingNondeterm_Router => Router}

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
}


object Semantics_RoutingNondeterm extends AbstractSemantics[Bag[Semantics_RoutingNondeterm_Data.Closure]] {
  import Substitution._
  import Crossproduct._
  import Semantics_RoutingNondeterm_Data._
  import djc.lang.{Semantics_RoutingNondeterm_Router => Router}

  def normalizeVal(v: Val) = v map (_.normalize)

  override def interp(p: Prog) = {
    Router.routeTable = collection.mutable.Map()
    interp(p, Map())
  }

  def interp(p: Prog, envServer: EnvServer): Res[Val] = p match {
    case Def(x, addr@ServerAddr(_), p)
      => interp(p, envServer + (x -> addr))
    case Def(x, ServerVar(y), p)
      => envServer get(y) match {
        case Some(s) => interp(p, envServer + (y -> s))
        case None => throw new IllegalArgumentException(s"Unbound server variable $y in ${Def(x, ServerVar(y), p)}")
      }
    case Def(x, s@ServerImpl(_), p)
      => {
      val addr = Router.registerServer(s)
      interp(p, envServer + (x -> ServerAddr(addr)))
    }
    case Par(ps) => {
      nondeterministic(
        crossProduct(ps map (interp(_, envServer))),
        (x: Val) => interpSends(x))
    }
    case s@Send(rcv, args) => interpSends(Bag(Closure(s, envServer)))
    case cl@Closure(s, env) => Set(Bag(cl))
  }

  def interpSends(v: Val): Res[Val] = {
    val canSend = selectSends(v)
    if (canSend.isEmpty)
      Set(v)
    else
      nondeterministic(
        canSend,
        (p: (ServerAddr, Rule, EnvServer, Map[Symbol, Service], Val)) => fireRule(p._1, p._2, p._3, p._4, p._5, v))
  }

  def selectSends(v: Val): Res[(ServerAddr, Rule, EnvServer, Map[Symbol, Service], Val)] =
    nondeterministic(
      (v map (collectRules(_))).flatten,
      (r: (ServerAddr, Rule, EnvServer)) =>
        matchRule(r._1, r._2.ps, v) map (x => (r._1, r._2, r._3, x._1, x._2))
    )

  def matchRule(server: ServerAddr, pats: Bag[Pattern], v: Val): Res[(Map[Symbol, Service], Val)] =
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

  def fireRule(server: ServerAddr, rule: Rule, env: EnvServer, subst: Map[Symbol, Service], used: Val, orig: Val): Res[Val] = {
    var p = map(substServer('this, server), rule.p)
    for ((x, s) <- subst)
      p = map(substService(x, s), p)
    val rest = orig diff used
    interp(Par(Bag(p) ++ rest), env)
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
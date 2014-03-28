//package djc.lang
//
//import scala.Symbol
//import djc.lang.Folder._
//import djc.lang.Mapper._
//import scala.language.postfixOps
//import util.Bag
//
//object Semantics_GroupedRoutingNondeterm extends AbstractSemantics[(Send, Map[Symbol, ServerVar])] {
//  import Substitution._
//
//  type Addr = String
//  type ServerAddr = ServerVar
//  object ServerAddr {
//    val prefix = "ADDR:"
//    def apply(addr: Addr) = new ServerAddr(Symbol(prefix + addr))
//    def unapply(s: ServerVar): Option[Addr] = getAddr(s.x.name)
//
//    def getAddr(name: String): Option[Addr] =
//      if (name.startsWith(prefix))
//        Some(name.substring(prefix.length))
//      else
//        None
//
//    def fromServer(s: Server, env: EnvServer): ServerAddr = s match {
//      case sa@ServerAddr(addr) => sa
//      case ServerVar(x) if env.isDefinedAt(x) => env(x)
//      case s@ServerImpl(_) => Router.registerServer(s)
//    }
//  }
//
//  type EnvServer = Map[Symbol, ServerAddr]
//
//  type Val = Closure
//  case class Closure(sends: Bag[Send], env: EnvServer)
//  def emptyVal = Closure(Bag(), Map())
//  def normalizeVal(v: Val) = {
//    var sends = v.sends
//    for ((k,v) <- v.env)
//      sends = sends map (map(substServer(k, Router.lookupAddr(v)), _).asInstanceOf[Send])
//    sends
//  }
//  def valData(v: Val): Bag[(Send, EnvServer)] = v.sends map ((_, v.env))
//  def addValData(v: Val, d: (Send, EnvServer)): Val =
//    if (v.env != d._2)
//      throw new IllegalArgumentException("Require equal environments")
//    else
//      Closure(v.sends + d._1, v.env)
//
//
//  object Router {
//    val routeTable = collection.mutable.Map[Addr, ServerImpl]()
//
//    var addrNum = 0
//    val addrPrefix = "Server@"
//    def nextAddr: Addr = {
//      addrNum += 1
//      val addr = addrPrefix + addrNum
//      if (!routeTable.isDefinedAt(addr))
//        addr
//      else
//        nextAddr
//    }
//
//    def registerServer(s: ServerImpl): ServerAddr = {
//      val addr = nextAddr
//      routeTable += (addr -> s)
//      ServerAddr(addr)
//    }
//
//    def lookupAddr(a: ServerAddr): ServerImpl = a match {
//      case ServerAddr(addr) => routeTable(addr)
//      case _ => throw new IllegalArgumentException(s"Not a server address: $a")
//    }
//  }
//
//  val servers = collection.mutable.Map[Addr,Val]()
//
//  def sendToServer(sa: ServerAddr, s: Send, env: EnvServer) =  {
//    val addr = ServerAddr.unapply(sa).get
//    val closure = servers.getOrElse(addr, emptyVal)
//    servers += (addr -> addValData(closure, (s, env)))
//  }
//
//  override def interp(p: Prog) = {
//    Router.routeTable.clear
//    interp(p, Map())
//  }
//
//  def interp(p: Prog, envServer: EnvServer): Res[Val] = p match {
//    case Def(x, addr@ServerAddr(_), p)
//      => interp(p, envServer + (x -> addr))
//    case Def(x, ServerVar(y), p)
//      => envServer get(y) match {
//        case Some(s) => interp(p, envServer + (y -> s))
//        case None => throw new IllegalArgumentException(s"Unbound server variable $y in ${Def(x, ServerVar(y), p)}")
//      }
//    case Def(x, s@ServerImpl(_), p)
//      => {
//      val addr = Router.registerServer(s)
//      interp(p, envServer + (x -> addr))
//    }
//    case Par(ps) => {
//      nondeterministic(
//        crossProduct(ps map (interp(_, envServer))),
//        (x: Val) => interpSends(x))
//    }
//    case s@Send(ServiceRef(srv, _), args) => {
//      sendToServer(ServerAddr.fromServer(srv, envServer), s, envServer)
//      interpSends(Closure(Bag(s), envServer))
//    }
//  }
//
//  def interpSends(v: Val): Res[Val] = {
//    val canSend = selectSends(v)
//    if (canSend.isEmpty)
//      Set(v)
//    else
//      nondeterministic(
//        canSend,
//        (p: (ServerAddr, Rule, Map[Symbol, Service], Val)) => fireRule(p._1, p._2, p._3, p._4, v))
//  }
//
//  def selectSends(v: Val): Res[(ServerAddr, Rule, Map[Symbol, Service], Val)] =
//    nondeterministic(
//      (v.sends map (collectRules(_, v.env))).flatten,
//      (r: (ServerAddr, Rule)) =>
//        matchRule(r._1, r._2.ps, v) map (x => (r._1, r._2, x._1, x._2))
//    )
//
//  def matchRule(server: ServerAddr, pats: Bag[Pattern], v: Val): Res[(Map[Symbol, Service], Val)] =
//    if (pats.isEmpty)
//      Set((Map(), emptyVal))
//    else {
//      val name = pats.head.name
//      val params = pats.head.params
//      val matchingSends = v.sends.filter({
//        case Send(ServiceRef(s, `name`), args)
//          if params.size == args.size && serverEqual(server, s, v.env) => true
//        case _ => false
//      })
//      nondeterministic(
//        matchingSends,
//        (s: Send) => matchRule(server, pats.tail, Closure(v.sends - s, v.env)) map (
//          p => (p._1 ++ (params zip s.args), Closure(p._2.sends + s, p._2.env))
//        )
//      )
//    }
//
//  def serverEqual(s1: Server, s2: Server, env: EnvServer): Boolean = (s1, s2) match {
//    case (ServerAddr(addr1), ServerAddr(addr2)) if addr1 == addr2 => true
//    case (ServerVar(x1), ServerVar(x2)) if x1 == x2 => true
//    case (ServerVar(x1), s2) if env.isDefinedAt(x1) => serverEqual(env(x1), s2, env)
//    case (s1, ServerVar(x2)) if env.isDefinedAt(x2) => serverEqual(s1, env(x2), env)
//    case (s1@ServerImpl(_), s2@ServerAddr(_)) => serverEqual(s1, Router.lookupAddr(s2), env)
//    case (s1@ServerAddr(_), s2@ServerImpl(_)) => serverEqual(Router.lookupAddr(s1), s2, env)
//    case (s1@ServerImpl(_), s2@ServerImpl(_)) => s1 == s2
//  }
//
//  def fireRule(server: ServerAddr, rule: Rule, subst: Map[Symbol, Service], used: Val, orig: Val): Res[Val] = {
//    var p = map(substServer('this, server), rule.p)
//    for ((x, s) <- subst)
//      p = map(substService(x, s), p)
//    val rest = orig.sends diff used.sends
//    interp(Par(Bag(p) ++ rest), orig.env)
//  }
//
//  def collectRules(s: Send, envServer: EnvServer): Bag[(ServerAddr, Rule)] = s match {
//    case Send(ServiceRef(s@ServerImpl(rules), _), _) => {
//      val addr = Router.registerServer(s)
//      rules map ((addr, _))
//    }
//    case Send(ServiceRef(ServerAddr(_), _), _) => {
//      val addr = s.rcv.asInstanceOf[ServiceRef].srv.asInstanceOf[ServerAddr]
//      val rules = Router.lookupAddr(addr).rules
//      rules map ((addr, _))
//    }
//    case Send(ServiceRef(ServerVar(x), _), _) if envServer.isDefinedAt(x) => {
//      val addr = envServer(x)
//      val rules = Router.lookupAddr(addr).rules
//      rules map ((addr, _))
//    }
//    case Send(_, _) => Bag()
//  }
//}
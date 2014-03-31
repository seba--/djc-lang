package djc.lang

import scala.Symbol
import scala.language.postfixOps
import util.Bag
import djc.lang.Mapper._
import scala.Some
import djc.lang.Semantics_ParallelRoutingConcurrent_Data.ServerAddr

object Semantics_ParallelRoutingConcurrent_Router {
  type Addr = String

  var routeTable: collection.mutable.Map[Addr, ServerThread] = null

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

  def registerServer(s: ServerThread): Addr = {
    val addr = nextAddr
    routeTable += (addr -> s)
    addr
  }

  def lookupAddr(addr: Addr): ServerThread = routeTable(addr)
}

object Semantics_ParallelRoutingConcurrent_Data {
  import Substitution._
  import djc.lang.{Semantics_ParallelRoutingConcurrent_Router => Router}

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
  def lookupAddr(a: ServerAddr): ServerThread = a match {
    case ServerAddr(addr) => Router.lookupAddr(addr)
    case _ => throw new IllegalArgumentException(s"Not a server address: $a")
  }

  type EnvServer = Map[Symbol, ServerAddr]
  case class ClosureProg(prog: Prog, env: EnvServer) extends Prog
  case class Closure(send: Send, env: EnvServer) {
    def normalize = env.toList.reverse.foldLeft(send)((s: Send, p: (Symbol, ServerAddr)) => map(substServer(p._1, lookupAddr(p._2).impl), s).asInstanceOf[Send])
  }

  type Servers = Map[Router.Addr, ServerThread]
}

class ServerThread(val impl: ServerImpl, val env: Map[Symbol, ServerAddr]) extends Thread {
  import Semantics_ParallelRoutingConcurrent_Data._

  var addr: ServerAddr = null

  var dirty = false
  var inbox = Bag[Closure]()
  var newMessages = Bag[Closure]()
  var terminate = false

  def sendRequest(cl: Closure) {
    synchronized {
      newMessages += cl
      dirty = true
    }
  }

  override def run() {
    while (!terminate) {
      if (dirty) {
        synchronized {
          inbox ++= newMessages
          newMessages = Bag()
          dirty = false
        }
        tryFireRule()
      }
      else
        Thread.sleep(1)
    }
  }

  def tryFireRule() {
    import Semantics_ParallelRoutingConcurrent._

    for (r <- impl.rules) {
      val canSend = matchRule(r.ps, inbox)
      if (!canSend.isEmpty) {
        val (subst, used) = canSend.head
        val (newProg, newInbox) = fireRule(addr, r, subst, used, inbox)
        inbox = newInbox
        interp(newProg, env)
        return
      }

    }
  }

  def normalizeVal: Bag[Send] = {
    var res: Bag[Send] = null
    synchronized {
      res = (inbox map (_.normalize)) ++ (newMessages map (_.normalize))
    }
    res
  }
}

object Semantics_ParallelRoutingConcurrent extends AbstractSemantics[Unit] { // all data is in the global state
  import Substitution._
  import Crossproduct._
  import Semantics_ParallelRoutingConcurrent_Data._
  import djc.lang.{Semantics_ParallelRoutingConcurrent_Router => Router}

  def normalizeVal(v: Val) = ((Bag() ++ Router.routeTable.values) map (_.normalizeVal)).flatten

  override def interp(p: Prog): Res[Val] = {
    Router.routeTable = collection.mutable.Map()
    interp(p, Map())
    Thread.sleep(50)
    Router.routeTable.values.map(_.terminate = true)
    Set(Unit)
  }

  def interp(p: Prog, envServer: EnvServer): Res[Val] = p match {
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

package djc.lang.sem.concurrent_6_thread


import djc.lang.Syntax._
import util.Bag
import djc.lang.sem.Substitution

/**
 * Created by seba on 09/04/14.
 */
object Data {
  type Env = Map[Symbol, Value]

  abstract class Value {
    def toNormalizedProg: Exp
    def toNormalizedResolvedProg: Exp
  }

  case object UnitVal extends Value {
    def toNormalizedProg = Par()
    def toNormalizedResolvedProg = Par()
  }

  case class ServerVal(addr: ServerAddr) extends Value {
    def toNormalizedProg = addr
    def toNormalizedResolvedProg = {
      val impl = lookupAddr(addr).impl
      val env = lookupAddr(addr).env
      env.foldLeft(impl) {
        case (srv, (x, value)) => Substitution(x, value.toNormalizedResolvedProg)(srv).asInstanceOf[ServerImpl]
      }
    }
  }

  case class ServiceVal(srv: ServerVal, x: Symbol) extends Value {
    def toNormalizedProg = ServiceRef(srv.toNormalizedProg, x)
    def toNormalizedResolvedProg = ServiceRef(srv.toNormalizedResolvedProg, x)
  }

  case class Match(subst: Map[Symbol, Value], used: Bag[SendVal])

  case class SendVal(rcv: ServiceVal, args: List[Value]) {
    def toNormalizedResolvedProg = Send(rcv.toNormalizedResolvedProg, args map (_.toNormalizedResolvedProg))
  }

  type ServerAddr = Var

  object ServerAddr {
    val prefix = "ADDR:"

    def apply(addr: Router.Addr) = new ServerAddr(Symbol(prefix + addr))

    def unapply(s: Var): Option[Router.Addr] = getAddr(s.x.name)

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


  type Servers = Map[Router.Addr, ServerThread]
  val emptyServers: Servers = Map()
  def sendToServer(servers: Servers, addr: Router.Addr, sv: SendVal): Servers = {
    servers(addr).sendRequest(sv)
    servers
  }


  case class ExpClosure(p: Exp, env: Env) extends Exp

}

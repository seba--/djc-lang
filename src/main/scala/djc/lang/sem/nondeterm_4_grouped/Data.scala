package djc.lang.sem.nondeterm_4_grouped


import util.Bag
import djc.lang.sem.Substitution
import djc.lang.Syntax._

object Data {
  type Env = Map[Symbol, Value]

  case class GroupedValue(v: Value, ss: Servers) {
    def toProg = v match {
      case UnitVal => Par(Bag[Exp]() ++ ss.values.flatten.map(_.toProg))
    }
  }

  abstract class Value {
    def toProg: Exp
    def toNormalizedProg: Exp
  }
  case object UnitVal extends Value {
    def toNormalizedProg = Par()
    def toProg = Par()
  }
  case class ServerVal(addr: ServerAddr) extends Value {
    def toNormalizedProg = lookupAddr(addr).normalize
    def toProg = addr
  }
  case class ServiceVal(srv: ServerVal, x: Symbol) extends Value {
    def toNormalizedProg = ServiceRef(srv.toNormalizedProg, x)
    def toProg = ServiceRef(srv.toProg, x)
  }

  case class ServerClosure(srv: ServerImpl, env: Env) {
    private[this] var addr_ : ServerAddr = null
    def addr_=(a: Router.Addr) = { addr_ = ServerAddr(a) }
    def addr = addr_
    def normalize = env.foldLeft(srv) {
      case (srv1, (x, value)) => Substitution(x, value.toNormalizedProg)(srv1).asInstanceOf[ServerImpl]
    }
  }

  case class Match(subst: Map[Symbol, Value], used: Bag[SendVal])

  case class SendVal(rcv: ServiceVal, args: List[Value]) {
    def toNormalizedProg = Send(rcv.toNormalizedProg, args map (_.toNormalizedProg))
    def toProg = Send(rcv.toProg, args map (_.toProg))
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
  def lookupAddr(a: ServerAddr): ServerClosure = a match {
    case ServerAddr(addr) => Router.lookupAddr(addr)
    case _ => throw new IllegalArgumentException(s"Not a server address: $a")
  }


  type Servers = Map[Router.Addr, Bag[SendVal]]
  val emptyServers: Servers = Map()
  def sendToServer(servers: Servers, addr: Router.Addr, sv: SendVal): Servers = {
    servers + (addr -> (servers.getOrElse(addr, Bag()) + sv))
  }
}

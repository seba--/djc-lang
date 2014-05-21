package djc.lang.sem.nondeterm_3_routed

import util.Bag
import djc.lang.FlatSyntax._
import djc.lang.sem.FlatSubstitution.Subst


object Data {
  type Env = Map[Symbol, Value]

  abstract class Value {
    def toProg: Prog
    def toNormalizedProg: Prog
  }
  case class UnitVal(sends: Bag[SendVal]) extends Value {
    def toProg = Par(sends.map(_.toSend.asInstanceOf[Prog]))
    def toNormalizedProg = Par(sends.map(_.toNormalizedProg.asInstanceOf[Prog]))
  }
  case class ServerVal(addr: ServerAddr) extends Value {
    def toProg = lookupAddr(addr)
    def toNormalizedProg = lookupAddr(addr).normalize
  }
  case class ServiceVal(srv: ServerVal, x: Symbol) extends Value {
    def toProg = ServiceRef(srv.toProg, x)
    def toNormalizedProg = ServiceRef(srv.toNormalizedProg, x)
  }

  case class ServerClosure(srv: ServerImpl, env: Env) extends Prog {
    private[this] var addr_ : ServerAddr = null
    def addr_=(a: Router.Addr) = { addr_ = ServerAddr(a) }
    def addr = addr_
    def normalize = env.foldLeft(srv) {
      case (srv1, (x, value)) => Subst(x, value.toNormalizedProg)(srv1).asInstanceOf[ServerImpl]
    }
  }

  case class Match(subst: Map[Symbol, Value], used: Bag[SendVal])

  case class SendVal(rcv: ServiceVal, args: List[Value]) {
    def toSend = Send(rcv.toProg, args map (_.toProg))
    def toNormalizedProg = Send(rcv.toNormalizedProg, args map (_.toNormalizedProg))
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

}

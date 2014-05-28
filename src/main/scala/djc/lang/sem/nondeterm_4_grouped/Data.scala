package djc.lang.sem.nondeterm_4_grouped


import util.Bag
import djc.lang.sem.Substitution
import djc.lang.Syntax._

import Router._

object Data {
  type Env = Map[Symbol, Value]

  abstract class Value {
    def toNormalizedProg: Exp
    def toNormalizedResolvedProg: Exp
  }

  case class ServerClosure(srv: ServerImpl, env: Env) {
    private[this] var addr_ : ServerAddr = null
    def addr_=(a: Router.Addr) = { addr_ = ServerAddr(a) }
    def addr = addr_
    def normalize = env.foldLeft(srv) {
      case (srv1, (x, value)) => Substitution(x, value.toNormalizedResolvedProg)(srv1).asInstanceOf[ServerImpl]
    }
  }
}
import Data._

class Data(router: Router) {

  case object UnitVal extends Value {
    def toNormalizedProg = Par()
    def toNormalizedResolvedProg = Par()
  }
  case class ServerVal(addr: ServerAddr) extends Value {
    def toNormalizedProg = addr
    def toNormalizedResolvedProg = router.lookupAddr(addr).normalize
  }
  case class ServiceVal(srv: ServerVal, x: Symbol) extends Value {
    def toNormalizedProg = ServiceRef(srv.toNormalizedProg, x)
    def toNormalizedResolvedProg = ServiceRef(srv.toNormalizedResolvedProg, x)
  }


  case class Match(subst: Map[Symbol, Value], used: Bag[SendVal])

  case class SendVal(rcv: ServiceVal, args: List[Value]) {
    def toNormalizedResolvedProg = Send(rcv.toNormalizedResolvedProg, args map (_.toNormalizedResolvedProg))
  }

  type Servers = Map[Router.Addr, Bag[SendVal]]

  val emptyServers: Servers = Map()
  def sendToServer(servers: Servers, addr: Router.Addr, sv: SendVal): Servers = {
    servers + (addr -> (servers.getOrElse(addr, Bag()) + sv))
  }
}

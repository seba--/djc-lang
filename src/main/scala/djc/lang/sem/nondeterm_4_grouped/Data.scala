package djc.lang.sem.nondeterm_4_grouped


import util.Bag
import djc.lang.sem.Substitution
import djc.lang.Syntax._

import Router._

object Data {
  type Env = Map[Symbol, Value]

  case class ServerClosure(srv: ServerImpl, env: Env) extends Value {
    def toExp = env.foldLeft(srv) {
      case (srv1, (x, value)) => Substitution(x, value.toExp)(srv1).asInstanceOf[ServerImpl]
    }
  }

  trait IServerVal extends Value {
    def addr: ServerAddr
  }

  trait IServiceVal {
    def srv: IServerVal
    def x: Symbol
  }

  trait ISendVal {
    def rcv: IServiceVal
    def args: List[Value]
    def toExp: Send
  }

  type BagMap[K,V] = Bag[(K,V)]
  type SetMap[K,V] = Set[(K,V)]

  type ServerSends = BagMap[Router.Addr, ISendVal]
  val noSends: ServerSends = Bag()
  def sendToServer(servers: ServerSends, addr: Router.Addr, sv: ISendVal): ServerSends = {
    servers + (addr -> sv)
  }
}
import Data._

class Data(router: Router) {

  case object UnitVal extends Value {
    def toExp = Par()
  }
  case class ServerVal(addr: ServerAddr) extends Value with IServerVal {
    def toExp = addr
  }
  case class ServiceVal(srv: ServerVal, x: Symbol) extends Value with IServiceVal {
    def toExp = ServiceRef(srv.toExp, x)
  }

  case class Match(subst: Env, used: Bag[(Router.Addr,ISendVal)])

  case class SendVal(rcv: ServiceVal, args: List[Value]) extends ISendVal {
    def toExp = Send(rcv.toExp, args map (_.toExp))
  }

  object resolveExp extends Mapper {
    override def map(prog: Exp): Exp = prog match {
      case addr@ServerAddr(_) => SpawnAny(map(router.lookupAddr(addr).toExp))
      case prog => super.map(prog)
    }
  }
}

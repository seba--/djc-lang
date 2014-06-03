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

  case class ServerClosure(srv: ServerImpl, env: Env) extends Value {
    def toNormalizedProg = toNormalizedResolvedProg
    def toNormalizedResolvedProg = env.foldLeft(srv) {
      case (srv1, (x, value)) => Substitution(x, value.toNormalizedResolvedProg)(srv1).asInstanceOf[ServerImpl]
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
    def toNormalizedProg: Send
    def toNormalizedResolvedProg: Send
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

  case class BaseVal(b: BaseValue) extends Value {
    def toNormalizedProg = b.toExp
    def toNormalizedResolvedProg = b.toExp
  }
  case object UnitVal extends Value {
    def toNormalizedProg = Par()
    def toNormalizedResolvedProg = Par()
  }
  case class ServerVal(addr: ServerAddr) extends Value with IServerVal {
    def toNormalizedProg = addr
    def toNormalizedResolvedProg = Spawn(router.lookupAddr(addr).toNormalizedResolvedProg)
  }
  case class ServiceVal(srv: ServerVal, x: Symbol) extends Value with IServiceVal {
    def toNormalizedProg = ServiceRef(srv.toNormalizedProg, x)
    def toNormalizedResolvedProg = ServiceRef(srv.toNormalizedResolvedProg, x)
  }


  case class Match(subst: Env, used: Bag[(Router.Addr,ISendVal)])

  case class SendVal(rcv: ServiceVal, args: List[Value]) extends ISendVal {
    def toNormalizedProg = Send(rcv.toNormalizedProg, args map (_.toNormalizedProg))
    def toNormalizedResolvedProg = Send(rcv.toNormalizedResolvedProg, args map (_.toNormalizedResolvedProg))
  }

  def makeBaseValue(v: Value) = v match {
    case BaseVal(b) => b
    case v => WrappedBaseValue(v)
  }

  def unmakeBaseValue(b: BaseValue): Value = b match {
    case b: WrappedBaseValue[Value @unchecked] => b.v
    case v => BaseVal(v)
  }
}

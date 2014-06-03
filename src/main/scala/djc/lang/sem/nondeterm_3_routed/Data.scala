package djc.lang.sem.nondeterm_3_routed

import util.Bag
import djc.lang.Syntax._
import djc.lang.sem.Substitution
import Router._

object Data {
  type Env = Map[Symbol, Value]

  abstract class Value {
    def toNormalizedProg: Exp
    def toNormalizedResolvedProg: Exp
  }
  case class ServerClosure(srv: ServerImpl, env: Env) extends Value{
    def toNormalizedProg = toNormalizedResolvedProg
    def toNormalizedResolvedProg = env.foldLeft(srv) {
      case (srv1, (x, value)) => Substitution(x, value.toNormalizedResolvedProg)(srv1).asInstanceOf[ServerImpl]
    }
  }
}
import Data._

class Data(router: Router) {

  case class BaseVal(b: BaseValue) extends Value {
    def toNormalizedProg = b.toExp
    def toNormalizedResolvedProg = b.toExp
  }
  case class UnitVal(sends: Bag[SendVal]) extends Value {
    def toNormalizedProg = Par(Bag[Exp]() ++ sends.map(_.toNormalizedProg))
    def toNormalizedResolvedProg = Par(Bag[Exp]() ++ sends.map(_.toNormalizedResolvedProg))
  }
  case class ServerVal(addr: ServerAddr) extends Value {
    def toNormalizedProg = addr
    def toNormalizedResolvedProg = Spawn(router.lookupAddr(addr).toNormalizedResolvedProg)
  }
  case class ServiceVal(srv: ServerVal, x: Symbol) extends Value {
    def toNormalizedProg = ServiceRef(srv.toNormalizedProg, x)
    def toNormalizedResolvedProg = ServiceRef(srv.toNormalizedResolvedProg, x)
  }

  case class Match(subst: Map[Symbol, Value], used: Bag[SendVal])

  case class SendVal(rcv: ServiceVal, args: List[Value]) {
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

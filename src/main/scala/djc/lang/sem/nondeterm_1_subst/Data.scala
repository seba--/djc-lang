package djc.lang.sem.nondeterm_1_subst

import djc.lang.Syntax._
import util.Bag
import djc.lang.Syntax.ServiceRef

object Data {
  abstract class Value {
    def toProg: Exp
  }
  case class BaseVal(v: BaseValue) extends Value {
    def toProg = v.toExp
  }
  case class UnitVal(sval: Bag[SendVal]) extends Value {
    def toProg = Par(sval.map(_.toSend.asInstanceOf[Exp]))
  }
  case class ServerVal(impl: ServerImpl) extends Value {
    def toProg = impl
  }
  case class ServiceVal(srv: ServerVal, x: Symbol) extends Value {
    def toProg = ServiceRef(srv.toProg, x)
  }

  case class SendVal(rcv: ServiceVal, args: List[Value]) {
    def toSend = Send(rcv.toProg, args map (_.toProg))
  }

  case class Match(subst: Map[Symbol, Value], used: Bag[SendVal])

  def makeBaseValue(v: Value) = v match {
    case BaseVal(b) => b
    case v => WrappedBaseValue(v)
  }

  def unmakeBaseValue(b: BaseValue): Value = b match {
    case b: WrappedBaseValue[Value @unchecked] => b.v
    case v => BaseVal(v)
  }
}

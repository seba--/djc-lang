package djc.lang.sem.nondeterm_1_subst

import djc.lang.Syntax._
import util.Bag
import djc.lang.Syntax.ServiceRef

object Data {
  def unmakeBaseValue(b: BaseValue) = b match {
    case WrappedNonBaseValue(v) => v
    case v => BaseVal(v)
  }
  case class WrappedNonBaseValue(v: Value) extends BaseValue {
    def toExp = v.toProg
  }

  abstract class Value {
    def toProg: Exp
    def makeBaseValue: BaseValue = WrappedNonBaseValue(this)
  }
  case class BaseVal(v: BaseValue) extends Value {
    def toProg = v.toExp
    override def makeBaseValue = v
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
}

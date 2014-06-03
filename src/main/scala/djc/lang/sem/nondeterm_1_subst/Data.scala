package djc.lang.sem.nondeterm_1_subst

import djc.lang.Syntax._
import util.Bag
import djc.lang.Syntax.ServiceRef

object Data {
  trait Value {
    def toProg: Exp
  }
  case class BaseVal(v: BaseValue) extends Value {
    def toProg = v.toExp
  }
  case class UnitVal(sval: Bag[SendVal]) extends Value {
    def toProg = Par(sval.map(_.toSend.asInstanceOf[Exp]))
  }
  class ServerVal(val impl: ServerImplVal, val n: Int) extends Spawn(false, impl.impl) with Value {
    def toProg = Spawn(impl.toProg)
    override def toString = s"ServerVal($impl, $n)"
  }
  object ServerVal {
    def apply(impl: ServerImplVal, n: Int) = new ServerVal(impl, n)
    def unapply(s: ServerVal) = Some((s.impl, s.n))
  }
  case class ServerImplVal(impl: ServerImpl) extends Value {
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

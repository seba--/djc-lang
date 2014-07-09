package djc.lang.sem.nondeterm_1_subst

import djc.lang.Syntax._
import util.Bag
import djc.lang.Syntax.ServiceRef

object Data {
  case class UnitVal(sval: Bag[SendVal]) extends Value {
    def toExp = Par(sval.map(_.toSend.asInstanceOf[Exp]))
  }
  class ServerVal(val impl: ServerImplVal, val n: Int) extends Spawn(false, impl.impl) with Value {
    def toExp = SpawnAny(impl.toExp)
    override def toString = s"ServerVal($impl, $n)"
  }
  object ServerVal {
    def apply(impl: ServerImplVal, n: Int) = new ServerVal(impl, n)
    def unapply(s: ServerVal) = Some((s.impl, s.n))
  }
  case class ServerImplVal(impl: ServerImpl) extends Value {
    def toExp = impl
  }
  case class ServiceVal(srv: ServerVal, x: Symbol) extends Value {
    def toExp = ServiceRef(srv.toExp, x)
  }

  case class SendVal(rcv: ServiceVal, args: List[Value]) {
    def toSend = Send(rcv.toExp, args map (_.toExp))
  }

  case class Match(subst: Map[Symbol, Value], used: Bag[SendVal])
}

package djc.lang.sem.nondeterm_1_subst

import djc.lang.FlatSyntax._
import util.Bag
import djc.lang.Syntax

object Data {
  abstract class Value
  case class UnitVal(sval: Bag[SendVal]) extends Value
  case class ServerVal(impl: ServerImpl) extends Value
  case class ServiceVal(srv: ServerVal, x: Symbol) extends Value

  case class SendVal(rcv: ServiceVal, args: List[ServiceVal]) {
    def toSend =
      Send(ServiceRef(rcv.srv.impl, rcv.x),
           args map (v => ServiceRef(v.srv.impl, v.x)))
  }

  case class Match(subst: Map[Symbol, ServiceVal], used: Bag[SendVal])
}

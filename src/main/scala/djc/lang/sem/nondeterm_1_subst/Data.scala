package djc.lang.sem.nondeterm_1_subst

import djc.lang.FlatSyntax._
import util.Bag
import djc.lang.Syntax

object Data {
  case class Value(pval: PVal, sval: Bag[SendVal])

  abstract class PVal
  case object UnitVal extends PVal
  case class ServerVal(impl: ServerImpl) extends PVal
  case class ServiceVal(srv: ServerVal, x: Symbol) extends PVal

  case class SendVal(rcv: ServiceVal, args: List[ServiceVal]) {
    def toSend =
      Send(ServiceRef(rcv.srv.impl, rcv.x),
           args map (v => ServiceRef(v.srv.impl, v.x)))
  }

  case class Match(subst: Map[Symbol, ServiceVal], used: Bag[SendVal])
}

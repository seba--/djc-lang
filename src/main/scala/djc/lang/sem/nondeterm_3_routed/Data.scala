package djc.lang.sem.nondeterm_3_routed

import util.Bag
import djc.lang.Syntax
import djc.lang.Syntax._
import djc.lang.sem.Substitution
import Router._

object Data {
  type Env = Map[Symbol, Value]

  case class ServerClosure(srv: ServerImpl, env: Env) extends Value {
    def toExp = env.foldLeft(srv) {
      case (srv1, (x, value)) => Substitution(x, value.toExp)(srv1).asInstanceOf[ServerImpl]
    }
  }
}

import Data._
class Data(router: Router) {

  case class UnitVal(sends: Bag[SendVal]) extends Value {
    def toExp = Par(Bag[Exp]() ++ sends.map(_.toExp))
  }
  case class ServerVal(addr: ServerAddr) extends Value {
    def toExp = addr
  }
  case class ServiceVal(srv: ServerVal, x: Symbol) extends Value {
    def toExp = ServiceRef(srv.toExp, x)
  }

  case class Match(subst: Map[Symbol, Value], used: Bag[SendVal])

  case class SendVal(rcv: ServiceVal, args: List[Value]) {
    def toExp = Send(rcv.toExp, args map (_.toExp))
  }

  object resolveExp extends Mapper {
    override def map(prog: Exp): Exp = prog match {
      case addr@ServerAddr(_) => SpawnAny(map(router.lookupAddr(addr).toExp))
      case prog => super.map(prog)
    }
  }
}

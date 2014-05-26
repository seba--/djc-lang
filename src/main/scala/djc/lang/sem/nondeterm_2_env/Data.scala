package djc.lang.sem.nondeterm_2_env

import scala.Symbol
import scala.language.postfixOps
import util.Bag
import djc.lang.Syntax._
import djc.lang.sem.Substitution.Subst

object Data {
  type Env = Map[Symbol, Value]

  abstract class Value {
    def toProg: Prog
    def toNormalizedProg: Prog
  }
  case class UnitVal(sends: Bag[SendVal]) extends Value {
    def toProg = Par(sends.map(_.toSend.asInstanceOf[Prog]))
    def toNormalizedProg = Par(sends.map(_.toNormalizedProg.asInstanceOf[Prog]))
  }
  case class ServerVal(impl: ServerImpl, env : Env) extends Value {
    def toProg = ServerClosure(impl, env)
    def toNormalizedProg = env.foldLeft(impl) {
      case (srv, (x, value)) => Subst(x, value.toNormalizedProg)(srv).asInstanceOf[ServerImpl]
    }
  }
  case class ServiceVal(srv: ServerVal, x: Symbol) extends Value {
    def toProg = ServiceRef(srv.toProg, x)
    def toNormalizedProg = ServiceRef(srv.toNormalizedProg, x)
  }

  case class ServerClosure(srv: ServerImpl, env: Env) extends Prog {
    def toValue = ServerVal(srv, env)
    def normalize = env.foldLeft(srv) {
      case (srv, (x, value)) => Subst(x, value.toNormalizedProg)(srv).asInstanceOf[ServerImpl]
    }
  }


  case class Match(subst: Map[Symbol, Value], used: Bag[SendVal])

  case class SendVal(rcv: ServiceVal, args: List[Value]) {
    def toSend = Send(rcv.toProg, args map (_.toProg))
    def toNormalizedProg = Send(rcv.toNormalizedProg, args map (_.toNormalizedProg))
  }
}

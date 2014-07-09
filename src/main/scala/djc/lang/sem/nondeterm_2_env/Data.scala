package djc.lang.sem.nondeterm_2_env

import scala.Symbol
import scala.language.postfixOps
import util.Bag
import djc.lang.Syntax._
import djc.lang.sem.Substitution

object Data {
  type Env = Map[Symbol, Value]

  case class UnitVal(sends: Bag[SendVal]) extends Value {
    def toExp = Par(Bag[Exp]() ++ sends.map(_.toExp))
  }
  case class ServerClosure(impl: ServerImpl, env: Env) extends Value {
    def toExp = env.foldLeft[Exp](impl) {
      case (srv, (x, value)) => Substitution(x, value.toExp)(srv)
    }
  }
  case class ServerVal(closure: ServerClosure, n: Int) extends Value {
    def toExp = closure.env.foldLeft[Exp](SpawnAny(closure.impl)) {
      case (srv, (x, value)) => Substitution(x, value.toExp)(srv)
    }
  }
  case class ServiceVal(srv: ServerVal, x: Symbol) extends Value {
    def toExp = ServiceRef(srv.toExp, x)
  }

  case class Match(subst: Map[Symbol, Value], used: Bag[SendVal])

  case class SendVal(rcv: ServiceVal, args: List[Value]) {
    def toExp = Send(rcv.toExp, args map (_.toExp))
  }
}

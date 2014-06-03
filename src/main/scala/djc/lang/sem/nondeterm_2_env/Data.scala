package djc.lang.sem.nondeterm_2_env

import scala.Symbol
import scala.language.postfixOps
import util.Bag
import djc.lang.Syntax._
import djc.lang.sem.Substitution

object Data {
  type Env = Map[Symbol, Value]

  abstract class Value {
    def toNormalizedProg: Exp
  }
  case class BaseVal(b: BaseValue) extends Value {
    def toNormalizedProg = b.toExp
  }
  case class UnitVal(sends: Bag[SendVal]) extends Value {
    def toNormalizedProg = Par(Bag[Exp]() ++ sends.map(_.toNormalizedProg))
  }
  case class ServerClosure(impl: ServerImpl, env: Env) extends Value {
    def toNormalizedProg = env.foldLeft[Exp](impl) {
      case (srv, (x, value)) => Substitution(x, value.toNormalizedProg)(srv)
    }
  }
  case class ServerVal(closure: ServerClosure, n: Int) extends Value {
    def toNormalizedProg = closure.env.foldLeft[Exp](SpawnAny(closure.impl)) {
      case (srv, (x, value)) => Substitution(x, value.toNormalizedProg)(srv)
    }
  }
  case class ServiceVal(srv: ServerVal, x: Symbol) extends Value {
    def toNormalizedProg = ServiceRef(srv.toNormalizedProg, x)
  }

  case class Match(subst: Map[Symbol, Value], used: Bag[SendVal])

  case class SendVal(rcv: ServiceVal, args: List[Value]) {
    def toNormalizedProg = Send(rcv.toNormalizedProg, args map (_.toNormalizedProg))
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

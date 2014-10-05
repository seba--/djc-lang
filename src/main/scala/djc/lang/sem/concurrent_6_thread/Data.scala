package djc.lang.sem.concurrent_6_thread


import djc.lang.Syntax._
import util.Bag
import djc.lang.sem.Substitution
import Router._

/**
 * Created by seba on 09/04/14.
 */
object Data {
  type Env = Map[Symbol, Value]

  trait ISendVal {
    def rcvAddr: ServerAddr
    def rcv: Value
    def args: List[Value]
    def toExp: Send
  }
}
import Data._

class Data(router: Router) {

  case object UnitVal extends Value {
    def toExp = Par()
  }

  case class ServerVal(addr: ServerAddr) extends Value {
    def toExp = addr
  }

  case class ServerClosure(impl: ServerImpl, env: Env) extends Value {
    def toExp = env.foldLeft(impl) {
        case (srv, (x, value)) => Substitution(x, value.toExp)(srv).asInstanceOf[ServerImpl]
      }
  }

  case class ServiceVal(srv: ServerVal, x: Symbol) extends Value {
    def toExp = ServiceRef(srv.toExp, x)
  }

  case class Match(subst: Env, used: Bag[ISendVal])

  case class SendVal(rcv: ServiceVal, args: List[Value]) extends ISendVal {
    def toExp = Send(rcv.toExp, args map (_.toExp))
    def rcvAddr = rcv.srv.addr
  }

  object resolveExp extends Mapper {
    override def map(prog: Exp): Exp = prog match {
      case addr@ServerAddr(_, _) => {
        val s = router.lookupServer(addr)
        SpawnAny(map(ServerClosure(s.impl, s.env).toExp))
      }
      case prog => super.map(prog)
    }
  }
}

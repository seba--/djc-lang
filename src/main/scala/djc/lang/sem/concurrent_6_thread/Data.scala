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

  abstract class Value {
    def toNormalizedProg: Exp
    def toNormalizedResolvedProg: Exp
  }

  trait ISendVal {
    def rcv: Value
    def args: List[Value]
    def toNormalizedResolvedProg: Send
  }
}
import Data._

class Data(router: Router) {

  case object UnitVal extends Value {
    def toNormalizedProg = Par()
    def toNormalizedResolvedProg = Par()
  }

  case class ServerVal(addr: ServerAddr) extends Value {
    def toNormalizedProg = addr
    def toNormalizedResolvedProg = {
      val scl = router.lookupAddr(addr)
      val impl = scl.impl
      val env = scl.env
      env.foldLeft(impl) {
        case (srv, (x, value)) => Substitution(x, value.toNormalizedResolvedProg)(srv).asInstanceOf[ServerImpl]
      }
    }
  }

  case class ServiceVal(srv: ServerVal, x: Symbol) extends Value {
    def toNormalizedProg = ServiceRef(srv.toNormalizedProg, x)
    def toNormalizedResolvedProg = ServiceRef(srv.toNormalizedResolvedProg, x)
  }

  case class Match(subst: Map[Symbol, Value], used: Bag[ISendVal])

  case class SendVal(rcv: ServiceVal, args: List[Value]) extends ISendVal {
    def toNormalizedResolvedProg = Send(rcv.toNormalizedResolvedProg, args map (_.toNormalizedResolvedProg))
  }

  type Servers = Map[Router.Addr, ServerThread]
  val emptyServers: Servers = Map()

  case class ExpClosure(p: Exp, env: Env) extends Exp

}

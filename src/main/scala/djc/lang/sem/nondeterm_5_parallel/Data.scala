package djc.lang.sem.nondeterm_5_parallel

import util.Bag
import djc.lang.sem.Substitution
import djc.lang.Syntax._

import Router._

object Data {
  type Env = Map[Symbol, Value]

  case class ServerClosure(srv: ServerImpl, env: Env) extends Value {
    def toExp = env.foldLeft(srv) {
      case (srv1, (x, value)) => Substitution(x, value.toExp)(srv1).asInstanceOf[ServerImpl]
    }
  }

  trait IServerVal extends Value {
    def addr: ServerAddr
  }

  trait IServiceVal {
    def srv: IServerVal
    def x: Symbol
  }

  trait ISendVal {
    def rcv: IServiceVal
    def args: List[Value]
    def toExp: Send
  }

  type BagMap[K,V] = Bag[(K,V)]
  type SetMap[K,V] = Set[(K,V)]

  type ServerSends = BagMap[Router.Addr, ISendVal]
  val noSends: ServerSends = Bag()
  def sendToServer(servers: ServerSends, addr: Router.Addr, sv: ISendVal): ServerSends = {
    servers + (addr -> sv)
  }
}
import Data._

class Data(router: Router) {
  case object UnitVal extends Value {
    def toExp = Par()
  }

  case class ServerVal(addr: ServerAddr) extends Value with IServerVal {
    def toExp = addr
  }

  case class ServiceVal(srv: ServerVal, x: Symbol) extends Value with IServiceVal {
    def toExp = ServiceRef(srv.toExp, x)
  }


  case class Match(subst: Env, used: Bag[(Router.Addr,ISendVal)])

  case class SendVal(rcv: ServiceVal, args: List[Value]) extends ISendVal {
    def toExp = Send(rcv.toExp, args map (_.toExp))
  }

  case class ExpClosure(p: Exp, env: Env) extends Exp

  object FlattenParWithExpClosure extends Mapper {
    override def map(e: Exp) = e match {
      case Par(es) => Par(flattenPars(es))
      case ExpClosure(p, env) => map(p) match {
        case Par(es) => Par(Bag[Exp]() ++ es.map(ExpClosure(_, env)))
        case e => ExpClosure(e, env)
      }
      case e => super.map(e)
    }

    def flattenPars(es: Bag[Exp]) =
      es flatMap (map(_) match {case Par(es) => es; case e => Bag(e)})
  }

  object resolveExp extends Mapper {
    override def map(prog: Exp): Exp = prog match {
      case addr@ServerAddr(_) => SpawnAny(map(router.lookupAddr(addr).toExp))
      case prog => super.map(prog)
    }
  }
}
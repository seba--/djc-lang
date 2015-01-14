package djc.lang.sem.concurrent_6_thread


import djc.lang.Syntax
import djc.lang.Syntax._
import util.Bag
import djc.lang.sem.Substitution
import Router._

import scala.collection.immutable.Queue

/**
 * Created by seba on 09/04/14.
 */
object Data {
  type Env = Map[Symbol, Value]

  case object UnitVal extends Value {
    def toExp = Par()
  }

  case object NULLVal extends Value {
    def toExp = NULL
  }

  case class ServerVal(addr: Symbol) extends Value {
    def toExp = Syntax.Addr(addr)
  }

  case class ServerClosure(impl: ServerImpl, env: Env) extends Value {
    def toExp = env.foldLeft(impl) {
        case (srv, (x, value)) => Substitution(x, value.toExp)(srv).asInstanceOf[ServerImpl]
      }
  }

  case class ImgVal(sc: ServerClosure, buffer: Bag[Request]) extends Value {
    def toExp =  Img(sc.toExp, buffer)
  }

  case class ServiceVal(srv: ServerVal, x: Symbol) extends Value {
    def toExp = ServiceRef(srv.toExp, x)
  }

  case class Match(subst: Env, used: Bag[Request])

  case class resolveExp(router: Router) extends Mapper {
    override def map(prog: Exp): Exp = prog match {
      case a@Addr(_) => {
        val s = router.lookupServer(a)
        SpawnAny(map(Img(ServerClosure(s.impl, s.env).toExp)))
      }
      case prog => super.map(prog)
    }
  }
}

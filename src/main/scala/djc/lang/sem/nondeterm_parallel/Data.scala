package djc.lang.sem.nondeterm_parallel

import djc.lang.sem.Substitution
import djc.lang.Prog
import djc.lang.Mapper._
import djc.lang.ServerImpl
import djc.lang.Send
import djc.lang.ServerVar
import scala.Some
import util.Bag

object Data {
  import Substitution._

  type ServerAddr = ServerVar
  object ServerAddr {
    val prefix = "ADDR:"
    def apply(addr: Router.Addr) = new ServerAddr(Symbol(prefix + addr))
    def unapply(s: ServerVar): Option[Router.Addr] = getAddr(s.x.name)

    def getAddr(name: String): Option[Router.Addr] =
      if (name.startsWith(prefix))
        Some(name.substring(prefix.length))
      else
        None
  }
  def lookupAddr(a: ServerAddr): ServerImpl = a match {
    case ServerAddr(addr) => Router.lookupAddr(addr)
    case _ => throw new IllegalArgumentException(s"Not a server address: $a")
  }

  type Env = Map[Symbol, ServerAddr]
  case class ClosureProg(prog: Prog, env: Env) extends Prog
  case class Closure(send: Send, env: Env) {
    def normalize = env.toList.reverse.foldLeft(send)((s: Send, p: (Symbol, ServerAddr)) => map(substServer(p._1, lookupAddr(p._2)), s).asInstanceOf[Send])
  }

  type Servers = Map[Router.Addr, Bag[Closure]]
  def sendToServer(servers: Servers, addr: Router.Addr, cl: Closure) =
    servers + (addr -> (servers(addr) + cl))
}

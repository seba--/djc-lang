package djc.lang.sem.nondeterm_4_grouped

import djc.lang.sem.Substitution
import djc.lang.Syntax._
import djc.lang.Syntax.Mapper._
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
  def lookupAddr(a: ServerAddr): ServerClosure = a match {
    case ServerAddr(addr) => Router.lookupAddr(addr)
    case _ => throw new IllegalArgumentException(s"Not a server address: $a")
  }

  type Env = Map[Symbol, ServerAddr]

  def normalizeProg(p: Prog, env: Env): Prog =
    env.foldLeft(p)((p: Prog, r: (Symbol, ServerAddr)) => {
      val server = lookupAddr(r._2)
      normalizeProg(map(Substitution.substServer(r._1, server.ths), p), server.env)
    })

  case class Match(subst: Map[Symbol, Service], used: Bag[SendClosure])

  case class SendClosure(send: Send, env: Env) {
    def normalize = normalizeProg(send, env).asInstanceOf[Send]
  }
  case class ServerClosure(ths: ServerImpl, env: Env)
  case class RuleClosure(rule: Rule, server: ServerAddr, env: Env)

  type Servers = Map[Router.Addr, Bag[SendClosure]]
  def sendToServer(servers: Servers, addr: Router.Addr, cl: SendClosure): Servers =
    servers + (addr -> (servers(addr) + cl))
}

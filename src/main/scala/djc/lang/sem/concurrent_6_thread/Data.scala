package djc.lang.sem.concurrent_6_thread

import djc.lang.sem.Substitution._
import djc.lang.Mapper._
import djc.lang._
import util.Bag

/**
 * Created by seba on 09/04/14.
 */
object Data {
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
  def lookupAddr(a: ServerAddr): ServerThread = a match {
    case ServerAddr(addr) => Router.lookupAddr(addr)
    case _ => throw new IllegalArgumentException(s"Not a server address: $a")
  }

  type Servers = Map[Router.Addr, ServerThread]
  type Env = Map[Symbol, ServerAddr]

  def normalizeProg(p: Prog, env: Env): Prog =
    env.foldLeft(p)((p: Prog, r: (Symbol, ServerAddr)) => {
      val server = lookupAddr(r._2)
      normalizeProg(map(substServer(r._1, server.impl), p), server.env)
    })

  case class Match(subst: Map[Symbol, Service], used: Bag[SendClosure])

  case class SendClosure(send: Send, env: Env) {
    def normalize = normalizeProg(send, env).asInstanceOf[Send]
  }
  case class ServerClosure(ths: ServerImpl, env: Env)
  case class RuleClosure(rule: Rule, server: ServerAddr, env: Env)

  case class ProgClosure(p: Prog, env: Env) extends Prog
}

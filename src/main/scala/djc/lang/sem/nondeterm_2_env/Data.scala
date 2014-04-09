package djc.lang.sem.nondeterm_2_env

import scala.Symbol
import scala.language.postfixOps
import util.Bag
import djc.lang.sem.Substitution
import djc.lang._
import djc.lang.Mapper._
import djc.lang.Send

object Data {
  type Env = Map[Symbol, ServerClosure]

  def normalizeProg(p: Prog, env: Env): Prog =
    env.foldLeft(p)((p: Prog, r: (Symbol, ServerClosure)) =>
      normalizeProg(map(Substitution.substServer(r._1, r._2.ths), p), r._2.env))

  case class Match(subst: Map[Symbol, Service], used: Bag[SendClosure])

  case class SendClosure(send: Send, env: Env) extends Prog {
    def normalize = normalizeProg(send, env).asInstanceOf[Send]
  }
  case class ServerClosure(ths: ServerImpl, env: Env)
  case class RuleClosure(rule: Rule, server: ServerImpl, env: Env)
}

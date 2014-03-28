package djc.lang

import scala.Symbol
import scala.language.postfixOps
import util.Bag
import scala.Some
import Mapper.map

object Semantics_EnvironmentNondeterm_Data {
  import Substitution._

  type EnvServer = Map[Symbol, ServerImpl]
  case class Closure(send: Send, env: EnvServer) extends Prog {
    def normalize =  env.toList.reverse.foldLeft(send)((s: Send, p: (Symbol, ServerImpl)) => map(substServer(p._1,p._2), s).asInstanceOf[Send])
  }
}

object Semantics_EnvironmentNondeterm extends AbstractSemantics[Bag[Semantics_EnvironmentNondeterm_Data.Closure]] {

  import Substitution._
  import Crossproduct._
  import Semantics_EnvironmentNondeterm_Data._

  def normalizeVal(v: Val) = v map (_.normalize)

  override def interp(p: Prog) = interp(p, Map())

  def interp(p: Prog, envServer: EnvServer): Res[Val] = p match {
    case Def(x, ServerVar(y), p)
      => envServer get(y) match {
        case Some(s) => interp(Def(x, s, p), envServer)
        case None => throw new IllegalArgumentException(s"Unbound server variable $y in ${Def(x, ServerVar(y), p)}")
      }
    case Def(x, s@ServerImpl(_), p)
      => interp(p, envServer + (x -> s))
    case Par(ps) => {
      nondeterministic(
        crossProduct(ps map (interp(_, envServer))),
        (x: Val) => interpSends(x))
    }
    case s@Send(rcv, args) => interpSends(Bag(Closure(s, envServer)))
    case cl@Closure(s, env) => Set(Bag(cl))
  }

  def interpSends(v: Val): Res[Val] = {
    val canSend = selectSends(v)
    if (canSend.isEmpty)
      Set(v)
    else
      nondeterministic(
        canSend,
        (p: (ServerImpl, Rule, EnvServer, Map[Symbol, Service], Val)) => fireRule(p._1, p._2, p._3, p._4, p._5, v))
  }

  def selectSends(v: Val): Res[(ServerImpl, Rule, EnvServer, Map[Symbol, Service], Val)] =
    nondeterministic(
      (v map (collectRules(_))).flatten,
      (r: (ServerImpl, Rule, EnvServer)) =>
        matchRule(r._1, r._2.ps, v) map (x => (r._1, r._2, r._3, x._1, x._2))
    )

  def matchRule(server: ServerImpl, pats: Bag[Pattern], v: Val): Res[(Map[Symbol, Service], Val)] =
    if (pats.isEmpty)
      Set((Map(), Bag()))
    else {
      val name = pats.head.name
      val params = pats.head.params
      val matchingSends = v.filter({
        case Closure(Send(ServiceRef(ServerVar(x), `name`), args), env)
          if env.isDefinedAt(x) && server == env(x)
          => params.size == args.size
        case Closure(Send(ServiceRef(server2@ServerImpl(_), `name`), args), env)
          if server == server2
          => params.size == args.size
        case _ => false
      })
      nondeterministic(
        matchingSends,
        (cl: Closure) => matchRule(server, pats.tail, v - cl) map (
          p => (p._1 ++ (params zip cl.send.args), p._2 + cl)
        )
      )
    }

  def fireRule(server: ServerImpl, rule: Rule, env: EnvServer, subst: Map[Symbol, Service], used: Val, orig: Val): Res[Val] = {
    var p = map(substServer('this, server), rule.p)
    for ((x, s) <- subst)
      p = map(substService(x, s), p)
    val restClosures = orig diff used
    interp(Par(Bag(p) ++ restClosures), env) // env has no effect on `restClosures`, but is needed for `p`
  }

  def collectRules(v: Closure): Bag[(ServerImpl, Rule, EnvServer)] = v match {
    case Closure(Send(ServiceRef(s@ServerImpl(rules), _), _), env) => rules map ((s, _, env))
    case Closure(Send(ServiceRef(ServerVar(x), _), _), env) if env.isDefinedAt(x) => {
      val s = env(x)
      s.rules map ((s, _, env))
    }
    case Closure(Send(_, _), _) => Bag()
  }
}
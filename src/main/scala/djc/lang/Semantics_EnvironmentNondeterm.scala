package djc.lang

import scala.Symbol
import djc.lang.Folder._
import djc.lang.Mapper._
import scala.language.postfixOps
import util.Bag

object Semantics_EnvironmentNondeterm extends AbstractSemantics {
  import Substitution._

  type EnvServer = Map[Symbol, ServerImpl]

  type Val = Closure
  case class Closure(sends: Bag[Send], env: EnvServer)
  def emptyVal = Closure(Bag(), Map())
  def normalizeVal(v: Val) = {
    var sends = v.sends
    for ((k,v) <- v.env)
      sends = sends map (map(substServer(k, v), _).asInstanceOf[Send])
    sends
  }


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
      val psvals: Bag[Res[Bag[Send]]] = ps map (interp(_, envServer) map (_.sends))
      nondeterministic(
        crossProduct(psvals) map (Closure(_, envServer)),
        (x: Val) => interpSends(x))
    }
    case s@Send(rcv, args) => interpSends(Closure(Bag(s), envServer))
  }

  def interpSends(v: Val): Res[Val] = {
    val canSend = selectSends(v)
    if (canSend.isEmpty)
      Set(v)
    else
      nondeterministic(
        canSend,
        (p: (ServerImpl, Rule, Map[Symbol, Service], Val)) => fireRule(p._1, p._2, p._3, p._4, v))
  }

  def selectSends(v: Val): Res[(ServerImpl, Rule, Map[Symbol, Service], Val)] =
    nondeterministic(
      (v.sends map (collectRules(_, v.env))).flatten,
      (r: (ServerImpl, Rule)) =>
        matchRule(r._1, r._2.ps, v) map (x => (r._1, r._2, x._1, x._2))
    )

  def matchRule(server: ServerImpl, pats: Bag[Pattern], v: Val): Res[(Map[Symbol, Service], Val)] =
    if (pats.isEmpty)
      Set((Map(), emptyVal))
    else {
      val name = pats.head.name
      val params = pats.head.params
      val matchingSends = v.sends.filter({
        case Send(ServiceRef(ServerVar(x), `name`), args)
          if v.env.isDefinedAt(x) && server == v.env(x)
          => params.size == args.size
        case Send(ServiceRef(server2@ServerImpl(_), `name`), args)
          if server == server2
          => params.size == args.size
        case _ => false
      })
      nondeterministic(
        matchingSends,
        (s: Send) => matchRule(server, pats.tail, Closure(v.sends - s, v.env)) map (
          p => (p._1 ++ (params zip s.args), Closure(p._2.sends + s, p._2.env))
        )
      )
    }

  def fireRule(server: ServerImpl, rule: Rule, subst: Map[Symbol, Service], used: Val, orig: Val): Res[Val] = {
    var p = map(substServer('this, server), rule.p)
    for ((x, s) <- subst)
      p = map(substService(x, s), p)
    val rest = orig.sends diff used.sends
    interp(Par(Bag(p) ++ rest), orig.env)
  }

  def collectRules(s: Send, envServer: EnvServer): Bag[(ServerImpl, Rule)] = s match {
    case Send(ServiceRef(s@ServerImpl(rules), _), _) => rules map ((s, _))
    case Send(ServiceRef(ServerVar(x), _), _) if envServer.isDefinedAt(x) => {
      val s = envServer(x)
      s.rules map ((s, _))
    }
    case Send(_, _) => Bag()
  }
}
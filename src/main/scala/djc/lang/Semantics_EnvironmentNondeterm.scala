package djc.lang

import scala.Symbol
import scala.language.postfixOps
import util.Bag
import scala.Some
import Mapper.map

object Semantics_EnvironmentNondeterm_Data {
  import Substitution._

  type Env = Map[Symbol, ServerClosure]

  def subst(p: Prog, env: Env): Prog =
    env.foldLeft(p)((p: Prog, r: (Symbol, ServerClosure)) => subst(map(substServer(r._1, r._2.ths), p), r._2.env))

  case class Match(subst: Map[Symbol, Service], used: Bag[SendClosure])

  case class SendClosure(send: Send, env: Env) extends Prog
  case class ServerClosure(ths: Server, env: Env)
  case class RuleClosure(rule: Rule, server: ServerClosure) {
//    def normalize =  env.reverse.fold(send)((s: Send, p: (Symbol, Server, Env)) => map(substServer(p._1,p._2), s).asInstanceOf[Send])
  }
}
import Semantics_EnvironmentNondeterm_Data._

object Semantics_EnvironmentNondeterm extends AbstractSemantics[Bag[SendClosure]] {

  import Substitution._
  import Crossproduct._

  def normalizeVal(v: Val) = v map (s => (subst(s.send, s.env).asInstanceOf[Send])) // map (_.normalize)

  override def interp(p: Prog) = interp(p, Map())

  def interp(p: Prog, env: Env): Res[Val] = p match {
    case Def(x, s@ServerVar(y), p) if env.isDefinedAt(y) =>
      interp(p, env + (x -> ServerClosure(s, env)))
    case Def(x, s@ServerImpl(_), p) =>
      interp(p, env + (x -> ServerClosure(s, env)))
    case Par(ps) => {
      nondeterministic(
        crossProduct(ps map (interp(_, env))),
        (x: Val) => interpSends(x))
    }
    case s@Send(rcv, args) => interpSends(Bag(SendClosure(s, env)))
    case sc@SendClosure(_, _) => interpSends(Bag(sc))
  }

  def interpSends(v: Val): Res[Val] = {
    val canSend = selectSends(v)
    if (canSend.isEmpty)
      Set(v)
    else
      nondeterministic(
        canSend,
        (p: (RuleClosure, Match)) => {
          val cl = p._1
          val ma = p._2
          val (prog, restSends) = fireRule(cl, ma, v)
          val ienv = cl.server.env + ('this -> cl.server)
          interp(Par(Bag(prog) ++ restSends), ienv) // env has no effect on `restSends`, but is needed for `p`
        })
  }

  def selectSends(v: Val): Res[(RuleClosure, Match)] =
    nondeterministic(
      (v map (collectRules(_))).flatten,
      (p: (Server, RuleClosure)) =>
        matchRule(p._1, p._2.rule.ps, v) map (x => (p._2, x))
    )

  def matchRule(server: Server, pats: Bag[Pattern], v: Val): Res[Match] =
    if (pats.isEmpty)
      Set(Match(Map(), Bag()))
    else {
      val name = pats.head.name
      val params = pats.head.params
      val matchingSends = v.filter({
        case SendClosure(Send(ServiceRef(`server`, `name`), args), _) => params.size == args.size
        case _ => false
      })
      nondeterministic(
        matchingSends,
        (s: SendClosure) => matchRule(server, pats.tail, v - s) map (
          p => Match(p.subst ++ (params zip s.send.args), p.used + s)
        )
      )
    }

  def fireRule(cl: RuleClosure, ma: Match, orig: Val): (Prog, Val) = {
    var p = cl.rule.p // map(substServer('this, server), rule.p)
    for ((x, s) <- ma.subst)
      p = map(substService(x, s), p)
    val restSends = orig diff ma.used
    (p, restSends)
  }

  def collectRules(v: SendClosure): Bag[(Server, RuleClosure)] = v match {
    case SendClosure(Send(ServiceRef(sv@ServerVar(x), y), args), env) if env.isDefinedAt(x) => {
      val sc = env(x)
      for ((v,r) <- collectRules(SendClosure(Send(ServiceRef(sc.ths, y), args), sc.env)))
        yield (sv, r)
    }
    case SendClosure(Send(ServiceRef(si@ServerImpl(rules), _), _), env) =>
      rules map ((r: Rule) => (si, RuleClosure(r, ServerClosure(si, env))))
  }
}
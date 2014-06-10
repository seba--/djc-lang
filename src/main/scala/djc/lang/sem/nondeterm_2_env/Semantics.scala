package djc.lang.sem.nondeterm_2_env

import djc.lang.sem.{ISemanticsFactory, Crossproduct, AbstractSemantics}
import util.Bag
import djc.lang.Syntax._
import Crossproduct._
import Data._
import djc.lang.FlattenPar.flattenPars

object Semantics extends AbstractSemantics[Value] with ISemanticsFactory[Value] {

  def newInstance() = this

  def normalizeVal(v: Val) = v.asInstanceOf[UnitVal].sends map (_.toNormalizedProg)

  val isFullyNondeterministic = true

  type Res[T] = Set[T]
  def resToSet[T](res: Res[T]) = res

  private var nextServerID = 0

  override def interp(p: Par) = interp(p, Map(), Bag())

  def interp(p: Exp, env: Env, sends: Bag[SendVal]): Res[Val] = p match {
    case BaseCall(b, es) =>
      nondeterministic[List[BaseValue], Val](
        crossProductList(es map (interp(_, env, Bag()) map (makeBaseValue(_)))),
        vs => Set(unmakeBaseValue(b.reduce(vs)))
      )

    case Var(y) if env.isDefinedAt(y) =>
      Set(env(y))

    case s@ServerImpl(_) =>
      Set(ServerClosure(s, env))

    case Spawn(_, e) =>
      nondeterministic[Val,Val](
        interp(e, env, sends),
        {case closure@ServerClosure(_, _) =>
          val id = nextServerID
          nextServerID += 1
          Set(ServerVal(closure, id))
        }
      )

    case ServiceRef(srv, x) =>
      nondeterministic[Val,Val](
        interp(srv, env, sends),
        {case sval@ServerVal(impl, env1)  =>
          Set(ServiceVal(sval, x))
        }
      )

    case Par(ps) =>
      nondeterministic[Bag[SendVal],Val](
        crossProduct(flattenPars(ps) map (interp(_, env, Bag()) map {case UnitVal(s) => s})),
        x => interpSends(sends ++ x))

    case Send(rcv, args) =>
      nondeterministic[Val,Val](
        interp(rcv, env, sends),
        { case svc@ServiceVal(srvVal, x) =>
            crossProductList(args map (interp(_, env, Bag()))) map (
              argvals => {
                var newSends = Bag[SendVal]()
                val normalizedArgvals = argvals.map {
                  case UnitVal(sends) => newSends ++= sends; UnitVal(Bag())
                  case v => v
                }
                UnitVal(sends ++ newSends + SendVal(svc, normalizedArgvals))
              }
            )
        }
      )

  }

  def interpSends(sends: Bag[SendVal]): Res[Val] = {
    val canSend = selectSends(sends)
    if (canSend.isEmpty)
      Set(UnitVal(sends))
    else
      nondeterministic[(ServerVal, Rule, Match), Val](
        canSend,
        {case (srv, r, m) =>
          val (newProg, newEnv, restSends) = fireRule(srv, r, m, sends)
          interp(newProg, newEnv, restSends)
        })
  }

  def selectSends(sends: Bag[SendVal]): Res[(ServerVal, Rule, Match)] =
    nondeterministic[(ServerVal, Rule), (ServerVal, Rule, Match)](
    (sends map collectRules).flatten,
    { case (srvVal, rule) => matchRule(srvVal, rule.ps, sends) map (x => (srvVal, rule, x)) }
    )

  def matchRule(server: ServerVal, pats: Bag[Pattern], sends: Bag[SendVal]): Res[Match] =
    if (pats.isEmpty)
      Set(Match(Map(), Bag()))
    else {
      val name = pats.head.name
      val params = pats.head.params
      val matchingSends = sends.filter({
        case SendVal(ServiceVal(`server`, `name`), args) => params.size == args.size
        case _ => false
      })
      nondeterministic[SendVal, Match](
        matchingSends,
        s => matchRule(server, pats.tail, sends - s) map (
          p => Match(p.subst ++ (params zip s.args), p.used + s)
          )
      )
    }

  def fireRule(server: ServerVal, rule: Rule, ma: Match, orig: Bag[SendVal]): (Exp, Env, Bag[SendVal]) = {
    val env = server.closure.env ++ ma.subst + ('this -> server)
    val rest = orig diff ma.used
    (Par(rule.p), env, rest)
  }

  def collectRules(s: SendVal): List[(ServerVal, Rule)] = {
    s.rcv.srv.closure.impl.rules map ((s.rcv.srv, _))
  }
}

package djc.lang.sem.nondeterm_1_subst

import scala.language.postfixOps
import util.Bag
import djc.lang.sem._
import djc.lang.Syntax._
import Data._
import djc.lang.sem.SemanticException
import djc.lang.sem.Substitution
import djc.lang.sem.nondeterm_1_subst.Data.Match
import djc.lang.sem.nondeterm_1_subst.Data.UnitVal
import djc.lang.Syntax.BaseCall
import djc.lang.sem.nondeterm_1_subst.Data.SendVal
import djc.lang.Syntax.Var
import djc.lang.sem.nondeterm_1_subst.Data.ServiceVal
import djc.lang.sem.nondeterm_1_subst.Data.ServerVal
import djc.lang.Syntax.ServiceRef
import djc.lang.Syntax.Rule
import djc.lang.FlattenPar.flattenPars

object Semantics extends AbstractSemantics[Value] with ISemanticsFactory[Value] {
  import Crossproduct._

  def newInstance() = this

  def normalizeVal(v: Val) = v.asInstanceOf[UnitVal].sval map (_.toSend)

  val isFullyNondeterministic = true

  type Res[T] = Set[T]
  def resToSet[T](res: Res[T]) = res

  override def interp(p: Par) = interp(p, Bag())

  def interp(p: Exp, sends: Bag[SendVal]): Res[Val] = p match {
    case BaseCall(b, es) =>
      nondeterministic[List[BaseValue], Val](
        crossProductList(es map (interp(_, Bag()) map (makeBaseValue(_)))),
        vs => Set(unmakeBaseValue(b.reduce(vs)))
      )

    case Par(ps) =>
      nondeterministic[Bag[SendVal],Val](
        crossProduct(flattenPars(ps) map (interp(_, Bag()) map {case UnitVal(s) => s})),
        x => interpSends(sends ++ x))

    case Seq(Nil) =>
      Set(UnitVal(sends))
    case Seq(p :: Nil) =>
      interp(p, sends)
    case Seq(p :: ps) =>
      nondeterministic[Val, Val](
        interp(p, sends),
        {case UnitVal(sends) => interp(Seq(ps), sends)}
      )

    case s@Send(rcv, args) =>
      nondeterministic[Val,Val](
        interp(rcv, sends),
        {case rcvVal@ServiceVal(_, _) =>
          crossProductList(args map (interp(_, Bag()))) map (
            argvals => {
              var newSends = Bag[SendVal]()
              val normalizedArgvals = argvals.map {
                case UnitVal(sends) => newSends ++= sends; UnitVal(Bag())
                case v => v
              }
              UnitVal(sends ++ newSends + SendVal(rcvVal, normalizedArgvals))
            }
          )
        }
      )

    case Var(x) =>
      throw SemanticException(s"Free variable $x")

    case ServiceRef(srv, x) =>
      nondeterministic[Val,Val](
        interp(srv, sends),
        {case srv@ServerVal(_) => Set(ServiceVal(srv, x))}        //TODO check if service defined in srv
      )

    case impl@ServerImpl(_,_) =>
      Set(ServerVal(impl))
  }

  def interpSends(sends: Bag[SendVal]): Res[Val] = {
    val canSend = selectSends(sends)
    if (canSend.isEmpty)
      Set(UnitVal(sends))
    else
      nondeterministic[(ServerVal, Rule, Match), Val](
        canSend,
        p => {
          val (newProg, newQueue) = fireRule(p._1, p._2, p._3, sends)
          interp(newProg, newQueue)
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

  def fireRule(server: ServerVal, rule: Rule, ma: Match, orig: Bag[SendVal]): (Exp, Bag[SendVal]) = {
    var p = Substitution('this, server.toProg)(rule.p)
    for ((x, v) <- ma.subst)
      p = Substitution(x, v.toProg)(p)
    val rest = orig diff ma.used
    (Par(p), rest)
  }

  def collectRules(s: SendVal): Bag[(ServerVal, Rule)] = {
    val Send(ServiceRef(ServerImpl(rules, _), _), _) = s.toSend
    rules map ((s.rcv.srv, _))
  }
}
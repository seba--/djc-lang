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

  def normalizeVal(v: Val) = {
    v.asInstanceOf[UnitVal].sval map (_.toSend)
  }

  val isFullyNondeterministic = true

  type Res[T] = Set[T]
  def resToSet[T](res: Res[T]) = res

  override def interp(p: Par) = interp(p, Bag())

  private var nextServerID = 0

  def interp(p: Exp, sends: Bag[SendVal]): Res[Val] = p match {
    case BaseCall(b, es) =>
      nondeterministic[List[Value], Val](
        crossProductList(es map (interp(_, Bag()))),
        vs => b.reduce(vs) match {
          case Left(v) => Set(v)
          case Right(e) => interp(e, sends)
        }
      )

    case Par(ps) =>
      nondeterministic[Bag[SendVal],Val](
        crossProduct(flattenPars(ps) map (interp(_, Bag()) map {case UnitVal(s) => s})),
        x => interpSends(sends ++ x))

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
        {case srv@ServerVal(_,_) => Set(ServiceVal(srv, x))
         case v => throw new MatchError(s"Illegal ServiceRef for $x. Expected ServerVal(_,_) but got $v")
        }
      )

    case impl@ServerImpl(_) =>
      Set(ServerImplVal(impl))

    case Spawn(_, e) =>
      nondeterministic[Val,Val](
        interp(e, sends),
        {case impl@ServerImplVal(_) =>
          val id = nextServerID
          nextServerID += 1
          Set(ServerVal(impl, id))
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
    var p = Substitution('this, server)(rule.p)
    for ((x, v) <- ma.subst)
      p = Substitution(x, v.toExp)(p)
    val rest = orig diff ma.used
    (Par(p), rest)
  }

  def collectRules(s: SendVal): List[(ServerVal, Rule)] = {
    val rules = s.rcv.srv.impl.impl.rules
    rules map ((s.rcv.srv, _))
  }
}
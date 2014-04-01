package djc.lang.sem.nondeterm_subst

import scala.Symbol
import djc.lang.Mapper._
import scala.language.postfixOps
import util.Bag
import djc.lang.sem.{Substitution, Crossproduct, AbstractSemantics}
import djc.lang._
import djc.lang.Def
import djc.lang.Send
import djc.lang.ServiceRef
import djc.lang.Rule
import djc.lang.Pattern
import djc.lang.Par

object Semantics extends AbstractSemantics[Bag[Send]] {
  import Substitution._
  import Crossproduct._

  def normalizeVal(v: Val) = v

  override def interp(p: Prog): Res[Val] = p match {
    case Def(x, s, p)
      => interp(map(substServer(x, s), p))
    case Par(ps) => {
      nondeterministic(
        crossProduct(ps map (interp(_))),
        (x: Val) => interpSends(x))
    }
    case s@Send(_, _) => interpSends(Bag(s))
  }

  def interpSends(sends: Bag[Send]): Res[Val] = {
    val canSend = selectSends(sends)
    if (canSend.isEmpty)
      Set(sends)
    else
      nondeterministic(
        canSend,
        (p: (Server, Rule, Map[Symbol, Service], Bag[Send]))
          => fireRule(p._1, p._2, p._3, p._4, sends))
  }

  def selectSends(sends: Bag[Send]): Res[(Server, Rule, Map[Symbol, Service], Bag[Send])] =
    nondeterministic(
      (sends map (collectRules(_))).flatten,
      (r: (Server, Rule)) =>
        matchRule(r._1, r._2.ps, sends) map (x => (r._1, r._2, x._1, x._2))
    )

  def matchRule(server: Server, pats: Bag[Pattern], sends: Bag[Send]): Res[(Map[Symbol, Service], Bag[Send])] =
    if (pats.isEmpty)
      Set((Map(), Bag()))
    else {
      val name = pats.head.name
      val params = pats.head.params
      val matchingSends = sends.filter({
        case Send(ServiceRef(`server`, `name`), args) => params.size == args.size
        case _ => false
      })
      nondeterministic(
        matchingSends,
        (s: Send) => matchRule(server, pats.tail, sends - s) map (
          p => (p._1 ++ (params zip s.args), p._2 + s)
        )
      )
    }

  def fireRule(server: Server, rule: Rule, subst: Map[Symbol, Service], usedSends: Bag[Send], allSends: Bag[Send]): Res[Val] = {
    var p = map(substServer('this, server), rule.p)
    for ((x, s) <- subst)
      p = map(substService(x, s), p)
    val rest = allSends diff usedSends
    interp(Par(Bag(p) ++ rest))
  }

  def collectRules(s: Send): Bag[(Server, Rule)] = s match {
    case Send(ServiceRef(s@ServerImpl(rules), _), _) => rules map ((s, _))
    case Send(_, _) => Bag()
  }
}
package djc.lang.sem.nondeterm_1_subst

import scala.Symbol
import scala.language.postfixOps
import util.Bag
import djc.lang.sem.{FlatSubstitution, Crossproduct, AbstractSemantics}
import djc.lang.FlatSyntax._
import djc.lang.Syntax
import Data._

object Semantics extends AbstractSemantics[Value] {
  import FlatSubstitution._
  import Crossproduct._

  def normalizeVal(v: Value) = v.asInstanceOf[UnitVal].sval map (_.toSend.toSyntaxProg)

  override def interp(p: Syntax.Prog) = interp(p.toFlatSyntax, Bag[SendVal]())

  def interp(p: Prog, sends: Bag[SendVal]): Res[Value] = p match {
    case Def(x, s, p) =>
      nondeterministic[Value,Value](
        interp(s, sends),
        {case ServerVal(impl) => interp(p.map(subst(x, impl)), sends)}
      )
    case Par(ps) =>
      nondeterministic[Bag[SendVal],Value](
        crossProduct(ps map (interp(_, Bag()) map (_ match {case UnitVal(sends) => sends}))),
        x => interpSends(x))
    case s@Send(rcv, args) =>
      nondeterministic[Value,Value](
        interp(rcv, sends),
        {case rcvVal@ServiceVal(_, _) =>
           nondeterministic[List[ServiceVal], Value](
             crossProductList(args map (interp(_, Bag()) map {case v@ServiceVal(_, _) => v})),
             argvals => interpSends(Bag(SendVal(rcvVal, argvals)))
           )
        }
      )
    case Var(x) =>
      throw new IllegalStateException(s"Unbound variable $x")
    case ServiceRef(srv, x) =>
      nondeterministic[Value,Value](
        interp(srv, sends),
        {case srv@ServerVal(_) => Set(ServiceVal(srv, x))}
      )
    case impl@ServerImpl(_) => 
      Set(ServerVal(impl))
  }

  def interpSends(sends: Bag[SendVal]): Res[Value] = {
    val canSend = selectSends(sends)
    if (canSend.isEmpty)
      Set(UnitVal(sends))
    else
      nondeterministic[(ServerVal, Rule, Match), Value](
        canSend,
        p => {
          val (newProg, newQueue) = fireRule(p._1, p._2, p._3, sends)
          interp(newProg, newQueue)
        })
  }

  def selectSends(sends: Bag[SendVal]): Res[(ServerVal, Rule, Match)] =
    nondeterministic[(ServerVal, Rule), (ServerVal, Rule, Match)](
      (sends map (collectRules(_))).flatten,
      r => matchRule(r._1, r._2.ps, sends) map (x => (r._1, r._2, x))
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

  def fireRule(server: ServerVal, rule: Rule, ma: Match, orig: Bag[SendVal]): (Prog, Bag[SendVal]) = {
    var p = rule.p.map(subst('this, server.impl))
    for ((x, s) <- ma.subst)
      p = p.map(subst(x, ServiceRef(s.srv.impl, s.x)))
    val rest = orig diff ma.used
    (p, rest)
  }

  def collectRules(s: SendVal): Bag[(ServerVal, Rule)] = s.rcv.srv.impl.rules map ((s.rcv.srv, _))
}
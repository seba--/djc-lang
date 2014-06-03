package djc.lang

import util.Bag
import scala.collection.immutable.ListMap

import djc.lang.typ.Types._
import djc.lang.typ.SubstType

object TypedSyntax {

  abstract class Exp {
    def eraseType: Syntax.Exp
  }

  case class Par(ps: Bag[Exp]) extends Exp {
    override def eraseType = Syntax.Par(ps map (_.eraseType))
    override def toString =
      if (ps.isEmpty)
        "Par()"
      else
        s"Par(${ps.toString})"
  }
  object Par { def apply(ps: Exp*) = new Par(Bag(ps: _*))}

  case class Send(rcv: Exp, args: List[Exp]) extends Exp {
    override def eraseType = Syntax.Send(rcv.eraseType, args.map(_.eraseType))
  }

  object Send {
    def apply(rcv: Exp, args: Exp*) = new Send(rcv, List(args: _*))
  }

  case class Var(x: Symbol) extends Exp {
    override def eraseType = Syntax.Var(x)
  }

  case class ServiceRef(srv: Exp, x: Symbol) extends Exp {
    override def eraseType = Syntax.ServiceRef(srv.eraseType, x)
  }

  case class ServerImpl(rules: Bag[Rule]) extends Exp {
    override def eraseType = Syntax.ServerImpl(rules map (_.eraseType))

    lazy val signature = {
      import collection.mutable.{HashMap, MultiMap, Set}
      val mm = new HashMap[Symbol, Set[Type]] with MultiMap[Symbol, Type]
      for (Rule(ps, _) <- rules; Pattern(svcname, params) <- ps)
        mm.addBinding(svcname, TSvc(params.map(_._2).toList))

      val m =
        mm.foldLeft(Map[Symbol, Type]()) {
          (m, kv) =>
            val (svcname, types) = kv
            if (types.size > 1)
              throw SyntaxException(s"inconsistent type annotations for service $svcname : $types")
            m + (svcname -> types.last)
        }
      TSrvRep(m)
    }
  }
  object ServerImpl {
    def apply(rules: Rule*): ServerImpl = ServerImpl(Bag(rules: _*))
  }

  case class Spawn(local: Boolean, e: Exp) extends Exp {
    override def eraseType = Syntax.Spawn(local, e.eraseType)
  }
  object Spawn {
    def apply(e: Exp): Spawn = Spawn(false, e)
  }
  object Server {
    def apply(rules: Bag[Rule]): Spawn = Spawn(false, ServerImpl(rules))
    def apply(rules: Rule*): Spawn = Spawn(false, ServerImpl(Bag(rules: _*)))
  }
  object LocalServer {
    def apply(rules: Bag[Rule]): Spawn = Spawn(true, ServerImpl(rules))
    def apply(rules: Rule*): Spawn = Spawn(true, ServerImpl(Bag(rules: _*)))
  }

  case class TApp(p: Exp, t: Type) extends Exp {
    override def eraseType = p.eraseType
  }

  case class TAbs(alpha: Symbol, bound: Option[Type], p: Exp) extends Exp {
    override def eraseType = p.eraseType
  }
  object TAbs {
    def apply(alpha: Symbol, p: Exp): TAbs = TAbs(alpha, None, p)
    def apply(alpha: Symbol, bound: Type, p: Exp): TAbs = TAbs(alpha, Some(bound), p)
  }

  case class UnsafeCast(e: Exp, t: Type) extends Exp {
    override def eraseType = e.eraseType
  }

  case class UpCast(e: Exp, t: Type) extends Exp {
    override def eraseType = e.eraseType
  }

  case class Rule(ps: Bag[Pattern], p: Exp) {
    def eraseType = Syntax.Rule(ps map (_.eraseType), p.eraseType)

    lazy val rcvars: ListMap[Symbol, Type] = {
      val m = ListMap[Symbol, Type]()
      ps.foldLeft(m)(_ ++ _.params)
    }

    override def toString = s"Rule($ps  =>  $p)"
  }

  case class Pattern(name: Symbol, params: ListMap[Symbol, Type]) {
    def eraseType = Syntax.Pattern(name, params.keys.toList)
  }

  object Pattern {
    def apply(name: Symbol, param: (Symbol, Type)*) = new Pattern(name, ListMap(param: _*))
  }

  case class SyntaxException(msg: String) extends RuntimeException(msg)






  type BaseValue = Syntax.BaseValue
  abstract class BaseOp(val ts: List[Type], val res: Type) extends Syntax.BaseOp {
    def reduce(vs: List[Syntax.BaseValue]): Syntax.BaseValue
  }

  case class BaseCall(b: BaseOp, es: List[Exp]) extends Exp {
    def eraseType = Syntax.BaseCall(b, es map (_.eraseType))
  }
  object BaseCall { def apply(b: BaseOp, es: Exp*): BaseCall = BaseCall(b, List(es:_*)) }




  implicit def varSymbol(s: Symbol) = Var(s)
  implicit def varSymbolInfixExp(s: Symbol) = new InfixExp(Var(s))
  implicit def varSymbolInfixPattern(s: Symbol) = PatternSymbol(s)
  implicit def patternBag(p: Pattern) = Bag(p)
  implicit def infixPattern(p: Pattern) = InfixPattern(p)
//  implicit def infixSend(e: Send) = new InfixSend(e)
  implicit def infixExp(e: Exp) = new InfixExp(e)
  class InfixExp(val e1: Exp) {
    def ~>(s: Symbol) = ServiceRef(e1, s)
    def !!(es: Exp*) = Send(e1, List(es:_*))
    def apply(t: Type): Exp = TApp(e1, t)
    def apply(ts: Type*): Exp = ts.foldLeft(this)((e,t) => new InfixExp(e.apply(t))).e1
    def &&(e2: Exp): Exp = Par(e1, e2)
    def &&(e2s: Bag[Exp]): Exp = Par(e2s + e1)
    def cast(t: Type) = UpCast(e1, t)
  }
//  class InfixSend(e1: Send) extends InfixExp(e1) {
//    def >>(e2: Send) = TypedSyntaxDerived.SendSeq(e1, e2)
//  }
  case class PatternSymbol(s: Symbol) {
    def ?(ps: (Symbol, Type)*) = Pattern(s, ListMap(ps:_*))
  }
  case class InfixPattern(p1: Pattern) {
    def &&(ps: Bag[Pattern]) = ps + p1
  }

  def ?(ts: Type*) = TSvc(List(ts:_*))
  implicit def tvarSymbol(s: Symbol) = TVar(s)
  implicit def infixType(t: Type) = InfixType(t)
  case class InfixType(t: Type) {
    def apply(t2: Type): Type = t match {
      case TUniv(x, _, t1) => SubstType(x, t2)(t1)
      case _ => throw new IllegalArgumentException(s"Expect TUniv but got $t")
    }
    def apply(t2s: Type*): Type = t2s.foldLeft(this)((t, t2) => InfixType(t.apply(t2))).t

    def ++(t2: Type): Type = (t, t2) match {
      case (TSrvRep(svcs1), TSrvRep(svcs2)) =>
        if (!svcs1.keySet.intersect(svcs2.keySet).isEmpty)
          throw new IllegalArgumentException(s"Cannot build union of server types: Overlapping service declarations.")
        else
          TSrvRep(svcs1 ++ svcs2)
      case _ => throw new IllegalArgumentException(s"Cannot build union of server types: Expected server types but got ${(t,t2)}")
    }
  }




  trait Mapper {
    def apply(prog: Exp): Exp = map(prog)

    def apply(tpe: Type): Type = mapType(tpe)

    def map(prog: Exp): Exp = prog match {
      case Par(ps) =>
        Par(ps map map)
      case Send(p, args) =>
        Send(map(p), (args map map))
      case Var(x) =>
        Var(x)
      case ServiceRef(p1, x) =>
        ServiceRef(map(p1), x)
      case ServerImpl(rs) =>
        ServerImpl(rs map mapRule)
      case Spawn(local, e) =>
        Spawn(local, map(e))
      case TApp(p1, t) =>
        TApp(map(p1), mapType(t))
      case TAbs(alpha, bound1, p1) =>
        TAbs(alpha, bound1.map(mapType(_)), map(p1))
      case UnsafeCast(e, t) =>
        UnsafeCast(map(e), mapType(t))
      case UpCast(e, t) =>
        UpCast(map(e), mapType(t))
      case BaseCall(b, ps) =>
        BaseCall(b, ps map map)
    }

    def mapType(tpe: Type): Type = tpe match {
      case Unit => Unit
      case TSvc(ts) => TSvc((ts map mapType))
      case TSrvRep(svcs) => TSrvRep(svcs mapValues mapType)
      case TSrv(t) => TSrv(mapType(t))
      case TVar(alpha) => TVar(alpha)
      case TBase(name) => TBase(name)
      case TUniv(alpha, bound, tpe1) => TUniv(alpha, bound.map(mapType(_)), mapType(tpe1))
    }

    def mapRule(rule: Rule): Rule = {
      val Rule(ps, prog) = rule
      Rule(ps map mapPattern, map(prog))
    }

    def mapPattern(pattern: Pattern): Pattern = {
      val Pattern(name, params) = pattern
      val transformedParams = params.foldLeft(ListMap[Symbol, Type]()) {
        (m, kv) =>
          m + (kv._1 -> mapType(kv._2))
      }
      Pattern(name, transformedParams)
    }
  }

  trait Fold {
    def fold[T](init: T)(prog: Exp): T = prog match {
      case Par(ps) =>
        ps.foldLeft(init)(fold(_)(_))
      case Send(p, args) =>
        args.foldLeft(fold(init)(p))(fold(_)(_))
      case Var(x) =>
        init
      case ServiceRef(p1, x) =>
        fold(init)(p1)
      case ServerImpl(rs) =>
        rs.foldLeft(init)(foldRule(_)(_))
      case Spawn(local, e) =>
        fold(init)(e)
      case TApp(p1, t) =>
        fold(foldType(init)(t))(p1)
      case TAbs(alpha, bound1, p1) =>
        fold(bound1.map(foldType(init)(_)).getOrElse(init))(p1)
      case UnsafeCast(e, t) =>
        fold(foldType(init)(t))(e)
      case UpCast(e, t) =>
        fold(foldType(init)(t))(e)
      case BaseCall(b, ps) =>
        ps.foldLeft(init)(fold(_)(_))
    }

    def foldType[T](init: T)(tpe: Type): T = tpe match {
      case Unit =>
        init
      case TSvc(ts) =>
        ts.foldLeft(init)(foldType(_)(_))
      case TSrvRep(svcs) =>
        svcs.foldLeft(init) {
          (i, kv) => foldType(i)(kv._2)
        }
      case TSrv(t) => foldType(init)(t)
      case TVar(alpha) =>
        init
      case TBase(name) =>
        init
      case TUniv(alpha, bound1, tpe1) =>
        foldType(bound1.map(foldType(init)(_)).getOrElse(init))(tpe1)
    }

    def foldRule[T](init: T)(rule: Rule): T = {
      val Rule(ps, prog) = rule
      fold((ps.foldLeft(init)(foldPattern(_)(_))))(prog)
    }

    def foldPattern[T](init: T)(pattern: Pattern): T = {
      val Pattern(_, params) = pattern
      params.foldLeft(init) {
        (i, kv) => foldType(i)(kv._2)
      }
    }
  }
}

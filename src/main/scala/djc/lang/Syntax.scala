package djc.lang

import util.Bag

object Syntax {

  abstract class Exp

  case class Par(ps: Bag[Exp]) extends Exp {
    override def toString =
      if (ps.isEmpty)
        "Par()"
      else
        s"Par(${ps.toString})"
  }
  object Par { def apply(ps : Exp*): Par = new Par(Bag(ps:_*)) }

  case class Send(rcv: Exp, args: List[Exp]) extends Exp
  object Send { def apply(rcv: Exp, args: Exp*): Send = new Send(rcv, List(args:_*)) }

  case class Var(x: Symbol) extends Exp

  case class ServiceRef(srv: Exp, x: Symbol) extends Exp

  case class ServerImpl(rules: Bag[Rule]) extends Exp {
    lazy val services = rules flatMap (r => r.ps map (p => p.name))
  }
  object ServerImpl { def apply(rules: Rule*): ServerImpl = new ServerImpl(Bag(rules:_*)) }

  case class Spawn(local: Boolean, e: Exp) extends Exp {
    override def equals(a: Any) = a.isInstanceOf[SpawnAny] && a == this || a.isInstanceOf[Spawn] && {
      val o = a.asInstanceOf[Spawn]
      o.local == local && o.e == e
    }
    override def hashCode = e.hashCode
  }
  class SpawnAny(e: Exp) extends Spawn(false, e) {
    override def equals(a: Any) = a.isInstanceOf[Spawn] && a.asInstanceOf[Spawn].e == e
    override def hashCode = e.hashCode
  }
  object Spawn { def apply(e: Exp): Spawn = Spawn(false, e) }
  object SpawnAny { def apply(e: Exp): SpawnAny = new SpawnAny(e) }

  case class Rule(ps: Bag[Pattern], p: Exp)

  case class Pattern(name: Symbol, params: List[Symbol])
  object Pattern { def apply(name: Symbol, params: Symbol*): Pattern = new Pattern(name, List(params:_*)) }



//  case class WrappedNonBaseValue[T <: {def toProg: Exp}](t: T) extends BaseValue {
//    def toExp = t.toProg
//  }
  abstract class BaseValue {
    def toExp: Exp
  }
  case class WrappedBaseValue[V](v: V) extends BaseValue {
    def toExp = throw new UnsupportedOperationException
  }

  abstract class BaseOp {
    def reduce(vs: List[BaseValue]): BaseValue
  }
  case class BaseCall(b: BaseOp, es: List[Exp]) extends Exp










  trait Mapper {
    def apply(prog: Exp): Exp = map(prog)

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
      case BaseCall(b, es) =>
        BaseCall(b, es map map)
    }

    def mapRule(rule: Rule): Rule = {
      val Rule(ps, prog) = rule
      Rule(ps map mapPattern, map(prog))
    }

    def mapPattern(pattern: Pattern): Pattern = pattern
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
      case BaseCall(b, es) =>
        es.foldLeft(init)(fold(_)(_))
    }

    def foldRule[T](init: T)(rule: Rule): T = {
      val Rule(ps, prog) = rule
      fold((ps.foldLeft(init)(foldPattern(_)(_))))(prog)
    }

    def foldPattern[T](init: T)(pattern: Pattern): T = init
  }
}
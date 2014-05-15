package djc.lang

import util.Bag

object FlatSyntax {

  abstract class Prog

  case class Def(x: Symbol, s: Prog, p: Prog) extends Prog

  case class Par(ps: Bag[Prog]) extends Prog
  object Par {
    def apply(ps : Prog*): Par = new Par(Bag(ps:_*))
  }

  case class Send(rcv: Prog, args: List[Prog]) extends Prog
  object Send {
    def apply(rcv: Prog, args: Prog*): Send = new Send(rcv, List(args:_*))
  }

  case class Var(x: Symbol) extends Prog

  case class ServiceRef(srv: Prog, x: Symbol) extends Prog

  case class ServerImpl(rules: Bag[Rule]) extends Prog
  object ServerImpl {
    def apply(rules: Rule*): ServerImpl = new ServerImpl(Bag(rules:_*))
  }

  case class Rule(ps: Bag[Pattern], p: Prog)

  case class Pattern(name: Symbol, params: List[Symbol])
  object Pattern {
    def apply(name: Symbol, params: Symbol*): Pattern = new Pattern(name, List(params:_*))
  }

  trait Mapper {
    def apply(prog: Prog): Prog = map(prog)

    def map(prog: Prog): Prog = prog match {
      case Def(x, p1, p2) =>
        Def(x, map(p1), map(p2))
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
    }

    def mapRule(rule: Rule): Rule = {
      val Rule(ps, prog) = rule
      Rule(ps map mapPattern, map(prog))
    }

    def mapPattern(pattern: Pattern): Pattern = pattern
  }

  trait Fold {
    def fold[T](init: T)(prog: Prog): T = prog match {
      case Def(x, p1, p2) =>
        fold(fold(init)(p1))(p2)
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
    }

    def foldRule[T](init: T)(rule: Rule): T = {
      val Rule(ps, prog) = rule
      fold((ps.foldLeft(init)(foldPattern(_)(_))))(prog)
    }

    def foldPattern[T](init: T)(pattern: Pattern): T = init
  }
}
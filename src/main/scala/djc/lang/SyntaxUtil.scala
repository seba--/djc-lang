package djc.lang

import djc.lang.Syntax._

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
package djc.lang.sem

import djc.lang.Syntax._
import djc.lang.Gensym._

case class Substitution(x: Symbol, repl: Exp) extends Mapper {
  lazy val replVars = FreeVars(repl)

  override def map(p: Exp): Exp = p match {
    case Var(`x`) => repl

    case ServerImpl(rs) =>
      if (x == 'this)
        ServerImpl(rs)
      else
        ServerImpl(rs map mapRule)

    case _ => super.map(p)
  }

  override def mapRule(rule: Rule): Rule = {
    val Rule(ps, prog) = rule
    val boundNames = ps.flatMap(_.params).toList
    val conflictingNames = boundNames filter replVars
    val captureAvoiding = conflictingNames.isEmpty

    lazy val replacements = conflictingNames zip gensyms(conflictingNames, replVars)
    lazy val progfresh = replacements.foldLeft(prog) {
      case (p, (xn, yn)) => Substitution(xn, Var(yn))(p)
    }
    lazy val rename: Symbol => Symbol = replacements.toMap orElse {
      case s: Symbol => s
    }
    lazy val psfresh = ps map {
      pat => Pattern(pat.name, pat.params.map(rename))
    }
    val (psres, progres) = if (captureAvoiding) (ps, prog) else (psfresh, progfresh)

    if (boundNames contains x)
      rule
    else
      Rule(psres, map(progres))
  }
}

object FreeVars extends Fold {
  def apply(prog: Exp): Set[Symbol] = fold(Set[Symbol]())(prog)

  def fold(init: Set[Symbol])(prog: Exp): Set[Symbol] = prog match {
    case Var(x) =>
      init + x
    case ServerImpl(rs) =>
      rs.foldLeft(init)(foldRule(_)(_)) - 'this
    case _ => super.fold(init)(prog)
  }

  def foldRule(init: Set[Symbol])(rule: Rule): Set[Symbol] = {
    super.foldRule(init)(rule) -- rule.ps.flatMap(_.params)
  }
}

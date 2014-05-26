package djc.lang.sem

import djc.lang.Syntax._
import djc.lang.Gensym._

case class Substitution(x: Symbol, repl: Prog) extends Mapper {
  lazy val replVars = FreeVars(repl)

  override def map(p: Prog): Prog = p match {
    case Def(x2, s2, p2) =>
      val isThis = x == 'this
      val captureAvoiding = !replVars.contains(x2)
      lazy val x2fresh = gensym(x2, replVars)
      lazy val p2fresh = Substitution(x2, Var(x2fresh))(p2)
      val (x2res, p2res) = if (captureAvoiding) (x2,p2) else (x2fresh,p2fresh)

      if (isThis)
        Def(x2res, s2, map(p2res))
      else if (x == x2)
        Def(x2, map(s2), p2)
      else
        Def(x2res, map(s2), map(p2res))

    case Var(`x`) => repl

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
  def apply(prog: Prog): Set[Symbol] = fold(Set[Symbol]())(prog)

  def fold(init: Set[Symbol])(prog: Prog): Set[Symbol] = prog match {
    case Def(x, p1, p2) =>
      fold(fold(init)(p1) - 'this)(p2) - x
    case Var(x) =>
      init + x
    case _ => super.fold(init)(prog)
  }

  def foldRule(init: Set[Symbol])(rule: Rule): Set[Symbol] = {
    super.foldRule(init)(rule) -- rule.ps.flatMap(_.params)
  }
}

package djc.lang.typ

import djc.lang.Gensym._
import djc.lang.typ.Types._
import djc.lang.TypedSyntax._

case class SubstType(alpha: Symbol, repl: Type) extends Mapper {
  lazy val replTVars = FreeTypeVars(repl)

  override def map(prog: Exp): Exp = prog match {
    case TAbs(alpha1, p1) =>
      val captureAvoiding = !replTVars(alpha1)
      lazy val alpha1fresh = gensym(alpha1, replTVars)
      lazy val p1fresh = SubstType(alpha1, TVar(alpha1fresh))(p1)
      val (alpha1res, p1res) = if (captureAvoiding) (alpha1, p1) else (alpha1fresh, p1fresh)

      if (alpha == alpha1)
        prog
      else
        TAbs(alpha1res, map(p1res))

    case _ => super.map(prog)
  }

  override def mapType(tpe: Type): Type = tpe match {
    case TVar(alpha1) if alpha == alpha1 =>
      repl

    case TUniv(alpha1, tpe1) =>
      val captureAvoiding = !replTVars(alpha1)
      lazy val alpha1fresh = gensym(alpha1, replTVars)
      lazy val tpe1fresh = SubstType(alpha1, TVar(alpha1fresh))(tpe1)
      val (alpha1res, tpe1res) = if (captureAvoiding) (alpha1, tpe1) else (alpha1fresh, tpe1fresh)

      if (alpha == alpha1)
        tpe
      else
        TUniv(alpha1res, tpe1res)

    case _ => super.mapType(tpe)
  }


}

case class SubstProg(x: Symbol, repl: Exp) extends Mapper {
  lazy val replVars = FreeVars(repl)
  lazy val replTVars = FreeTypeVars(repl)

  override def map(prog: Exp): Exp = prog match {
    case Var(y) if x == y =>
      repl

    case ServerImpl(rs) =>
      if (x == 'this)
        ServerImpl(rs)
      else
        ServerImpl(rs map mapRule)

    case TAbs(alpha, p1) =>
      val captureAvoiding = !replTVars(alpha)
      lazy val alphafresh = gensym(alpha, replTVars)
      lazy val p1fresh = SubstType(alpha, TVar(alphafresh))(p1)
      val (alphares, p1res) = if (captureAvoiding) (alpha, p1) else (alphafresh, p1fresh)

      TAbs(alphares, map(p1res))

    case _ =>
      super.map(prog)
  }

  override def mapRule(rule: Rule): Rule = {
    val Rule(ps, prog) = rule
    val boundNames = rule.rcvars.toList map (_._1)
    val conflictingNames = boundNames filter replVars
    val captureAvoiding = conflictingNames.isEmpty

    lazy val replacements = conflictingNames zip gensyms(conflictingNames, replVars)
    lazy val progfresh = replacements.foldLeft(prog) {
      (p, kv) => SubstProg(kv._1, Var(kv._2))(p)
    }
    lazy val rename: Symbol => Symbol = replacements.toMap orElse {
      case s: Symbol => s
    }
    lazy val psfresh = ps map {
      pat =>
        Pattern(pat.name, pat.params.map {
          kv => rename(kv._1) -> kv._2
        })
    }
    val (psres, progres) = if (captureAvoiding) (ps, prog) else (psfresh, progfresh)

    if (boundNames contains x)
      rule
    else
      Rule(psres, map(progres))
  }

  override def mapType(tpe: Type): Type = tpe
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

  def foldType(init: Set[Symbol])(tpe: Type) = init

  def foldPattern(init: Set[Symbol])(pattern: Pattern) = init

  def foldRule(init: Set[Symbol])(rule: Rule): Set[Symbol] = {
    super.foldRule(init)(rule) -- rule.rcvars.keySet
  }
}

object FreeTypeVars extends Fold {
  def apply(prog: Exp): Set[Symbol] = fold(Set[Symbol]())(prog)

  def apply(tpe: Type): Set[Symbol] = foldType(Set[Symbol]())(tpe)

  def fold(init: Set[Symbol])(prog: Exp): Set[Symbol] = prog match {
    case TAbs(alpha, p1) =>
      fold(init)(p1) - alpha
    case _ => super.fold(init)(prog)
  }

  def foldType(init: Set[Symbol])(tpe: Type): Set[Symbol] = tpe match {
    case TVar(alpha) =>
      init + alpha
    case TUniv(alpha, tpe1) =>
      foldType(init)(tpe1) - alpha
    case _ => super.foldType(init)(tpe)
  }
}
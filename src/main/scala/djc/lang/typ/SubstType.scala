package djc.lang.typ

import djc.lang.Gensym._
import djc.lang.typ.Types._
import djc.lang.TypedSyntax._

trait SubstTypeFactory[+Subst <: SubstTypeTemplate] {
  def apply(substs: Map[Symbol, Type]): Subst
  def apply(substs: (Symbol,Type)*): Subst = apply(Map(substs:_*))
  def apply(substs: List[(Symbol,Type)]): Subst = apply(substs.toMap)
}

trait SubstTypeTemplate extends Mapper {
  val substs: Map[Symbol, Type]
  val mkSubst: SubstTypeFactory[SubstTypeTemplate]

  lazy val replTVars = substs.values.foldLeft(Set[Symbol]()) {
    case (s, t) => s ++ FreeTypeVars(t)
  }

  override def map: TMapE = {
    case p if substs.isEmpty => p

    case TAbs(alpha1, bound1, p1) if substs.contains(alpha1) =>
      TAbs(alpha1, mapType(bound1), mkSubst(substs - alpha1)(p1))

    case TAbs(alpha1, bound1, p1) =>
      val captureAvoiding = !replTVars(alpha1)
      lazy val alpha1fresh = gensym(alpha1, replTVars)
      lazy val p1fresh = mkSubst(alpha1 -> TVar(alpha1fresh))(p1)
      val (alpha1res, p1res) = if (captureAvoiding) (alpha1, p1) else (alpha1fresh, p1fresh)

      TAbs(alpha1res, mapType(bound1), map(p1res))

    case prog => super.map(prog)
  }

  override def mapType: TMapT = {
    case t if substs.isEmpty => t

    case TVar(alpha1) if substs.contains(alpha1) =>
      substs(alpha1)

    case TUniv(alpha1, bound1, tpe1) if substs.contains(alpha1) =>
      TUniv(alpha1, mapType(bound1), mkSubst(substs - alpha1)(tpe1))

    case TUniv(alpha1, bound1, tpe1) =>
      val captureAvoiding = !replTVars(alpha1)
      lazy val alpha1fresh = gensym(alpha1, replTVars)
      lazy val tpe1fresh = mkSubst(alpha1 -> TVar(alpha1fresh))(tpe1)
      val (alpha1res, tpe1res) = if (captureAvoiding) (alpha1, tpe1) else (alpha1fresh, tpe1fresh)

      TUniv(alpha1res, mapType(bound1), mapType(tpe1res))

    case tpe => super.mapType(tpe)
  }
}

class SubstType(val substs: Map[Symbol, Type]) extends SubstTypeTemplate {
  val mkSubst = SubstType
}

object SubstType extends SubstTypeFactory[SubstType] {
  def apply(substs: Map[Symbol, Type]): SubstType = new SubstType(substs.filter { case (k, TVar(v)) if k == v => false
  case _ => true})
}

abstract class SubstTemplate(x: Symbol, repl: Exp) extends Mapper {
  lazy val replVars = FreeVars(repl)
  lazy val replTVars = FreeTypeVars(repl)

  override def map: TMapE = {
    case Var(y) if x == y =>
      repl

    case ServerImpl(rs) =>
      if (x == 'this)
        ServerImpl(rs)
      else
        ServerImpl(rs map mapRule)

    case TAbs(alpha, bound1, p1) =>
      val captureAvoiding = !replTVars(alpha)
      lazy val alphafresh = gensym(alpha, replTVars)
      lazy val p1fresh = SubstType(alpha -> TVar(alphafresh))(p1)
      val (alphares, p1res) = if (captureAvoiding) (alpha, p1) else (alphafresh, p1fresh)

      TAbs(alphares, mapType(bound1), map(p1res))

    case prog =>
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

  override def mapType: TMapT = { case t => t }
}

case class SubstProg(x: Symbol, repl: Exp) extends SubstTemplate(x,repl)

trait FreeVarsTemplate extends Fold {
  def apply(prog: Exp): Set[Symbol] = fold(Set[Symbol]())(prog)

  def fold(init: Set[Symbol]): FoldE[Set[Symbol]] = {
    case Var(x) =>
      init + x
    case ServerImpl(rs) =>
      rs.foldLeft(init)(foldRule(_)(_)) - 'this
    case prog => super.fold(init)(prog)
  }

  def foldType(init: Set[Symbol])(tpe: Type) = init

  def foldPattern(init: Set[Symbol])(pattern: Pattern) = init

  def foldRule(init: Set[Symbol])(rule: Rule): Set[Symbol] = {
    super.foldRule(init)(rule) -- rule.rcvars.keySet
  }
}

object FreeVars extends FreeVarsTemplate

trait FreeTypeVarsTemplate extends Fold {
  def apply(prog: Exp): Set[Symbol] = fold(Set[Symbol]())(prog)

  def apply(tpe: Type): Set[Symbol] = foldType(Set[Symbol]())(tpe)

  def fold(init: Set[Symbol]): FoldE[Set[Symbol]] = {
    case TAbs(alpha, bound1, p1) =>
      foldType(fold(init)(p1) - alpha)(bound1)
    case prog => super.fold(init)(prog)
  }

  def foldType(init: Set[Symbol]): FoldT[Set[Symbol]] = {
    case TVar(alpha) =>
      init + alpha
    case TUniv(alpha, bound1, tpe1) =>
      foldType(foldType(init)(tpe1) - alpha)(bound1)
    case tpe => super.foldType(init)(tpe)
  }
}

object FreeTypeVars extends FreeTypeVarsTemplate

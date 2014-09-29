package djc.lang.typ

import djc.lang.Gensym._
import djc.lang._

object TypeOps {
  def captureAvoiding(F1: TypeFamily, F2: TypeFamily)(alpha: Symbol, tpe: F1.Type, beta: Symbol, tpe1: F2.Type): (Symbol, F1.Type, F2.Type) = {
    lazy val ftv = F1.op.freeTypeVars(tpe)
    lazy val ftv1 = F2.op.freeTypeVars(tpe1)
    val alphares = if (alpha != beta && ftv1(alpha))
      Gensym(alpha, ftv ++ ftv1)
    else alpha
    (alphares, F1.op.substType(alpha -> F1.TVar(alphares))(tpe), F2.op.substType(beta -> F2.TVar(alphares))(tpe1))
  }
}

trait TypeOps {
  val types: TypeFamily
  import types._

  def freeTypeVars: FreeTypeVars
  def substType(substs: Map[Symbol, Type]): SubstType
  def substType(substs: (Symbol, Type)*): SubstType = substType(Map(substs:_*))
  def substType(substs: List[(Symbol, Type)]): SubstType = substType(substs.toMap)

  def captureAvoiding(alpha: Symbol, tpe: Type, beta: Symbol, tpe1: Type): (Symbol, Type, Type) =
    TypeOps.captureAvoiding(types, types)(alpha, tpe, beta, tpe1)

  trait FreeTypeVars extends StrictFold[Set[Symbol]] {
    def apply(tpe: Type): Set[Symbol] = foldType(Set[Symbol]())(tpe)

    override def foldType(init: Set[Symbol]): FoldT = {
      case TVar(alpha) =>
        init + alpha
      case TUniv(alpha, bound1, tpe1) =>
        foldType(foldType(init)(tpe1) - alpha)(bound1)
      case tpe => super.foldType(init)(tpe)
    }
  }

  trait SubstType extends Mapper {
    val substs: Map[Symbol, Type]

    lazy val replTVars = substs.values.foldLeft(Set[Symbol]()) {
      case (s, t) => s ++ freeTypeVars(t)
    }

    override def mapType: TMapT = {
      case t if substs.isEmpty => t

      case TVar(alpha1) if substs.contains(alpha1) =>
        substs(alpha1)

      case TUniv(alpha1, bound1, tpe1) if substs.contains(alpha1) =>
        TUniv(alpha1, mapType(bound1), substType(substs - alpha1)(tpe1))

      case TUniv(alpha1, bound1, tpe1) =>
        val captureAvoiding = !replTVars(alpha1)
        lazy val alpha1fresh = gensym(alpha1, replTVars)
        lazy val tpe1fresh = substType(alpha1 -> TVar(alpha1fresh))(tpe1)
        val (alpha1res, tpe1res) = if (captureAvoiding) (alpha1, tpe1) else (alpha1fresh, tpe1fresh)

        TUniv(alpha1res, mapType(bound1), mapType(tpe1res))

      case tpe => super.mapType(tpe)
    }
  }
}

trait DefaultTypeOpsImpl extends TypeOps {
  import types._

  def freeTypeVars = FreeTypeVars
  protected object FreeTypeVars extends FreeTypeVars

  def substType(sub: Map[Symbol, Type]) = new SubstType {
    val substs: Map[Symbol, Type] =
      sub.filter { case (k, TVar(v)) if k == v => false
                   case _ => true}
  }
}

trait TypedSyntaxOps {
  self =>
  val syntax: TypedSyntaxFamily

  import syntax._
  import types._

  def freeTypeVars: FreeTypeVars
  def freeVars: FreeVars
  def substExp(x: Symbol, repl: Exp): Subst
  def substType(substs: Map[Symbol, Type]): SubstType
  def substType(substs: (Symbol, Type)*): SubstType = substType(Map(substs:_*))
  def substType(substs: List[(Symbol, Type)]): SubstType = substType(substs.toMap)

  trait FreeTypeVars extends syntax.StrictFold[Set[Symbol]] {
    val typeFold = types.op.freeTypeVars
    import typeFold._

    def apply(prog: Exp): Set[Symbol] = fold(Set[Symbol]())(prog)

    override def fold(init: Set[Symbol]): FoldE = {
      case TAbs(alpha, bound1, p1) =>
        foldType(fold(init)(p1) - alpha)(bound1)
      case prog => super.fold(init)(prog)
    }
  }

  trait SubstType extends syntax.Mapper {
    val typeMapper: types.op.SubstType
    import typeMapper.mapType
    lazy val replTVars = typeMapper.replTVars

    lazy val substs: Map[Symbol, Type] = typeMapper.substs

    override def map: TMapE = {
      case p if substs.isEmpty => p

      case TAbs(alpha1, bound1, p1) if substs.contains(alpha1) =>
        TAbs(alpha1, mapType(bound1), substType(substs - alpha1)(p1))

      case TAbs(alpha1, bound1, p1) =>
        val captureAvoiding = !replTVars(alpha1)
        lazy val alpha1fresh = gensym(alpha1, replTVars)
        lazy val p1fresh = substType(alpha1 -> TVar(alpha1fresh))(p1)
        val (alpha1res, p1res) = if (captureAvoiding) (alpha1, p1) else (alpha1fresh, p1fresh)

        TAbs(alpha1res, mapType(bound1), map(p1res))

      case prog => super.map(prog)
    }
  }

  trait FreeVars extends syntax.StrictFold[Set[Symbol]] {
    val typeFold = types.identityFold(Set[Symbol]())

    def apply(prog: Exp): Set[Symbol] = fold(Set())(prog)

    override def fold(init: Set[Symbol]): FoldE = {
      case Var(x) =>
        init + x
      case ServerImpl(rs) =>
        rs.foldLeft(init)(foldRule(_)(_)) - 'this
      case prog => super.fold(init)(prog)
    }

    override def foldPattern(init: Set[Symbol])(pattern: Pattern) = init

    override def foldRule(init: Set[Symbol])(rule: Rule): Set[Symbol] = {
      super.foldRule(init)(rule) -- rule.rcvars.keySet
    }
  }

  class Subst(x: Symbol, repl: Exp) extends syntax.Mapper {
    val typeMapper = types.identityMap
    lazy val replVars = freeVars(repl)
    lazy val replTVars = freeTypeVars(repl)

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
        lazy val p1fresh = substType(alpha -> TVar(alphafresh))(p1)
        val (alphares, p1res) = if (captureAvoiding) (alpha, p1) else (alphafresh, p1fresh)

        TAbs(alphares, typeMapper.mapType(bound1), map(p1res))

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
        (p, kv) => substExp(kv._1, Var(kv._2))(p)
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
  }
}

trait DefaultTypedSyntaxOps extends TypedSyntaxOps {
  import syntax.types.Type
  import syntax.Exp

  def freeTypeVars = FreeTypeVars
  def freeVars = FreeVars
  def substExp(x: Symbol, repl: Exp) = new Subst(x, repl)
  def substType(sub: Map[Symbol, Type]) = new SubstType {
    val typeMapper = syntax.types.op.substType(sub)
  }

  protected object FreeVars extends FreeVars
  protected object FreeTypeVars extends FreeTypeVars
}

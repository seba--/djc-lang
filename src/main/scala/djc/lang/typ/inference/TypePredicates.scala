package djc.lang.typ.inference

import djc.lang.typ.Checker.TVarContext
import djc.lang.typ.Types._
import util.ListOps._

/**
 * Variance and rigidity checks for types
 */
object TypePredicates {
  import op._
  implicit class TypePreds(val t: Type) extends AnyVal {
    def isConstantIn(tvar: Symbol): Boolean = TypePredicates.isConstantIn(t, tvar)
    def isCovariantIn(tvar: Symbol): Boolean = TypePredicates.isCovariantIn(t, tvar)
    def isContravariantIn(tvar: Symbol): Boolean = TypePredicates.isContravariantIn(t, tvar)
    def isInvariantIn(tvar: Symbol): Boolean = TypePredicates.isInvariantIn(t, tvar)
    def isRigidIn(tvar: Symbol): Boolean = TypePredicates.isRigidIn(t, tvar)
    def isRigidUnder(tgamma: TVarContext): Boolean = TypePredicates.isRigid(tgamma, t)
  }

  def isConstantIn(r: Type, tvar: Symbol): Boolean = !freeTypeVars(r)(tvar)

  def isCovariantIn(r: Type, tvar: Symbol): Boolean = r match {
    case TVar(alpha) =>
      alpha == tvar

    case TSvc(ts) =>
      val tvarTypes = ts.filter(!isConstantIn(_, tvar))
      tvarTypes.nonEmpty && tvarTypes.forall(isContravariantIn(_, tvar))

    case TSrv(t) =>
      isCovariantIn(t, tvar)

    case TSrvRep(svcs) =>
      val svcTypes = svcs.values.filter(!isConstantIn(_, tvar))
      svcTypes.nonEmpty && svcTypes.forall(isCovariantIn(_, tvar))

    case TUniv(alpha, bound, tpe) =>
      isConstantIn(bound, tvar) && alpha != tvar && isCovariantIn(tpe, tvar)

    case TPair(_, ts) =>
      val occurrences = ts.filter(!isConstantIn(_, tvar))
      occurrences.nonEmpty && occurrences.forall(isCovariantIn(_, tvar))

    case _ => false
  }

  def isContravariantIn(r: Type, tvar: Symbol): Boolean = r match {
    case TSrv(t) => isContravariantIn(t, tvar)

    case TSrvRep(svcs) =>
      val svcTypes = svcs.values.filter(!isConstantIn(_, tvar))
      svcTypes.nonEmpty && svcTypes.forall(isContravariantIn(_, tvar))

    case TSvc(ts) =>
      val tvarTypes = ts.filter(!isConstantIn(_, tvar))
      tvarTypes.nonEmpty && tvarTypes.forall(isCovariantIn(_, tvar))

    case TUniv(alpha, bound, tpe) =>
      isConstantIn(bound, tvar) && alpha != tvar && isContravariantIn(tpe, tvar)

    case TPair(_, ts) =>
      val occurrences = ts.filter(!isConstantIn(_, tvar))
      occurrences.nonEmpty && occurrences.forall(isContravariantIn(_, tvar))

    case _ => false
  }

  /**
   * A fold that checks if a type is invariant w.r.t. to the given type variable,
   * i.e. (i) the type variable occurs in the type and (ii)
   * all occurrences of tvar are in arguments of base types (which are not pairs).
   *
   * The fold returns two booleans. The first is true iff (i) holds.
   * The second is true iff (ii) holds.
   *
   * @param tvar
   */
  case class IsInvariantIn(tvar: Symbol) extends LazyFold[(Boolean, Boolean)] {
    def apply(t: Type): (Boolean, Boolean) = foldType((false, true))(t)

    override def foldType(init: => (Boolean, Boolean)): FoldT = {
      case TVar(`tvar`) => (true, false)
      case p@TPair(_, _) => super.foldType(init)(p)
      case TBase(_, ts) =>
        val nonTvarTs = ts.filter(_ != TVar(tvar))
        val occursHere = nonTvarTs.length != ts.length
        val (exists, invariant) = nonTvarTs.lazyFoldr((false, true)){
          case (t, state) =>
            val (ex1, inv1) = foldType(state)(t)
            if (!inv1)
              (ex1, inv1)
            else {
              val (ex2, inv2) = state
              (ex1 || ex2, inv2)
            }
        }

        (occursHere || exists, invariant)

      case t =>
        super.foldType(init)(t)
    }
  }

  def isInvariantIn(r: Type, tvar: Symbol): Boolean = {
    val (exists, allInvariant) = IsInvariantIn(tvar)(r)
    exists && allInvariant
  }

  /**
   * A fold that checks if a type is rigid w.r.t. to the given type variable,
   * i.e. (i) the type variable occurs in the type and (ii)
   * all occurrences are in bounds of universal types.
   *
   * The fold returns two booleans. The first is true iff (i) holds.
   * The second is true iff (ii) holds.
   *
   * @param tvar
   */
  case class IsRigidIn(tvar: Symbol) extends LazyFold[(Boolean, Boolean)] {
    def apply(t: Type): (Boolean, Boolean) = foldType((false, true))(t)

    override def foldType(init: => (Boolean, Boolean)): FoldT = {
      case TVar(`tvar`) => (init._1, false)
      case TUniv(alpha, bound, t) =>
        val occursInBound = freeTypeVars(bound)(tvar)

        if (alpha == tvar)
          (occursInBound || init._1, init._2)
        else {
          val (exists, invariant) = foldType(init)(t)
          (occursInBound || exists, invariant)
        }

      case t =>
        super.foldType(init)(t)
    }
  }

  def isRigidIn(r: Type, tvar: Symbol): Boolean = {
    val (exists, allRigid) = IsRigidIn(tvar)(r)
    exists && allRigid
  }

  def isBottomVar(tgamma: TVarContext)(alpha: Symbol): Boolean = tgamma(alpha) match {
    case Bot => true
    case TVar(beta) => isBottomVar(tgamma)(beta)
    case _ => false
  }

  def isRigid(tgamma: TVarContext, s: Type): Boolean = s match {
    case Top => true
    case Unit => true
    case Bot =>
      tgamma.values.forall(_ != Bot)
    case TVar(alpha) =>
      !isBottomVar(tgamma)(alpha)
    case TUniv(alpha, bound, tpe) =>
      isRigid(tgamma, bound) && isRigid(tgamma + (alpha -> bound), tpe)
    case TPair(_, ts) => ts.forall(isRigid(tgamma, _))
    case TBase(_, _) => true //TODO do we handle equivalence classes properly here?
    case TSrv(t) => isRigid(tgamma, t)
    case TSrvRep(svcs) => svcs.values.forall(isRigid(tgamma, _))
    case TSvc(ts) => ts.forall(isRigid(tgamma, _))
  }
}

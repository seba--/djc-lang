package djc.lang.typ.inference

import djc.lang.typ.Checker.TVarContext
import djc.lang.typ.Types._

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

    case _ => false
  }

  def isInvariantIn(r: Type, tvar: Symbol): Boolean =
    isContravariantIn(r, tvar) && isCovariantIn(r, tvar)

  def isRigidIn(r: Type, tvar: Symbol): Boolean = r match {
    case TBase(_, ts) =>
      ts.exists(!isConstantIn(_, tvar))

    case TSrv(t) => isRigidIn(t, tvar)

    case TSrvRep(svcs) =>
      val svcTypes = svcs.values.filter(!isConstantIn(_, tvar))
      svcTypes.nonEmpty && svcTypes.forall(isRigidIn(_, tvar))

    case TSvc(ts) =>
      val tvarTypes = ts.filter(!isConstantIn(_, tvar))
      tvarTypes.nonEmpty && tvarTypes.forall(isRigidIn(_, tvar))

    case TUniv(alpha, bound, _) if alpha == tvar =>
      !isConstantIn(bound, tvar)

    case TUniv(alpha, bound, tpe) =>
      !isConstantIn(bound, tvar) || isRigidIn(tpe, tvar)

    case _ => false
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
    case TBase(_, _) => true //TODO do we handle equivalence classes properly here?
    case TSrv(t) => isRigid(tgamma, t)
    case TSrvRep(svcs) => svcs.values.forall(isRigid(tgamma, _))
    case TSvc(ts) => ts.forall(isRigid(tgamma, _))
  }
}

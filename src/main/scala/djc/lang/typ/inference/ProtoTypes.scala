package djc.lang.typ.inference

import djc.lang.TypedSyntax.Fold
import djc.lang.typ.{FreeTypeVarsTemplate, SubstTypeFactory, SubstType}
import djc.lang.typ.Types._

/**
 * Partial types for colored type inference
 */
object ProtoTypes {
  //placeholder in partial type information
  case object Hole extends Type {
    override def ===(that: Type) = ???
  }
  //Variant of type substitution which works on partial types
  class SubstProtoType(substs: Map[Symbol, Type]) extends SubstType(substs) {
    override val mkSubst = SubstPrototype
    override def mapType: TMapT = {
      case Hole => Hole
      case t => super.mapType(t)
    }
  }
  object SubstPrototype extends SubstTypeFactory[SubstProtoType] {
    def apply(substs: Map[Symbol, Type]) = new SubstProtoType(substs.filter { case (k, TVar(v)) if k == v => false
    case _ => true})
  }

  object FreeProtoTypeVars extends FreeTypeVarsTemplate {
    override def foldType(init: Set[Symbol]): FoldT[Set[Symbol]] = {
      case Hole => init
      case tpe => super.foldType(init)(tpe)
    }
  }

  object IsPrototype extends Fold {
    def apply(t: Type): Boolean = foldType(false)(t)

    def foldType(init: Boolean): FoldT[Boolean] = {
      case Hole => true
      case t => super.foldType(init)(t)
    }
  }

}

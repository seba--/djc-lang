package djc.lang.typ.inference

import djc.lang.typ._

/**
 * Partial types for colored type inference
 */
trait ProtoTypesFamily extends TypeFamily {
  self: TypeOps =>

  //placeholder in partial type information
  case object Hole extends Type {
    override def ===(that: Type) = ???
    override def toFamily(TF: TypeFamily) = ???
  }

  trait Mapper extends super.Mapper {
    override def mapType = {
      case Hole => Hole
      case t => super.mapType(t)
    }
  }

  trait StrictFold[T] extends super.StrictFold[T] {
    override def foldType(init: T): FoldT = {
      case Hole => init
      case t => super.foldType(init)(t)
    }
  }
}

trait ProtoTypeOps extends DefaultTypeOpsImpl {
  self: ProtoTypesFamily =>

  def isPrototype(t: Type): Boolean = IsPrototype(t)

  object IsPrototype extends LazyFold[Boolean] {
    def apply(t: Type): Boolean = foldType(false)(t)

    override def foldType(init: => Boolean): FoldT = {
      case Hole => true
      case t =>
        super.foldType(init)(t)
    }
  }
}

object ProtoTypes extends ProtoTypesFamily with ProtoTypeOps

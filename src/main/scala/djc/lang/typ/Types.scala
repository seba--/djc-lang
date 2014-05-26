package djc.lang.typ

object Types {
  abstract class Type {
    /**
     * Equality modulo bound vars
     */
    def ===(that: Type): Boolean = this == that
  }

  case object Unit extends Type

  case class TSvc(params: List[Type]) extends Type {
    override def ===(that: Type) = that match {
      case TSvc(params1) => params.corresponds(params1)(_ === _)
      case _ => false
    }
  }

  object TSvc {
    def apply(params: Type*): TSvc = TSvc(List(params: _*))
  }

  case class TSrv(svcs: Map[Symbol, Type]) extends Type {
    override def ===(that: Type) = that match {
      case TSrv(svcs1) => svcs.keySet == svcs1.keySet && svcs.forall {
        case (s, tpe) => tpe === svcs1(s)
      }
      case _ => false
    }
  }

  object TSrv {
    def apply(svcs: (Symbol, Type)*): TSrv = TSrv(Map(svcs: _*))
  }

  case class TVar(alpha: Symbol) extends Type

  case class TBase(name: Symbol) extends Type

  case class TUniv(alpha: Symbol, tpe: Type) extends Type {
    override def ===(that: Type) = that match {
      case TUniv(beta, tpe1) => tpe === SubstType(beta, TVar(alpha))(tpe1)
      case _ => false
    }
  }
}
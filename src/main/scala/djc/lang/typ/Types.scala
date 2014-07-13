package djc.lang.typ

object Types {
  abstract class Type {
    /**
     * Equality modulo bound vars
     */
    def ===(that: Type): Boolean = this == that
  }

  case object Top extends Type
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

  case class TSrvRep(svcs: Map[Symbol, Type]) extends Type {
    override def ===(that: Type) = that match {
      case TSrvRep(svcs1) => svcs.keySet == svcs1.keySet && svcs.forall {
        case (s, tpe) => tpe === svcs1(s)
      }
      case _ => false
    }
  }
  object TSrvRep {
    def apply(svcs: (Symbol, Type)*): TSrvRep = TSrvRep(Map(svcs: _*))
  }

  case class TSrv(rep: Type) extends Type {
    override def ===(that: Type) = that match {
      case TSrv(rep2) => rep === rep2
      case _ => false
    }
  }
  

  case class TVar(alpha: Symbol) extends Type

  case class TBase(name: Symbol, ts: List[Type]) extends Type {
    override def ===(that: Type) = that match {
      case TBase(`name`, ts1) =>
        ts.corresponds(ts1)(_ === _)
      case _ => false
    }
  }
  object TBase {
    def apply(name: Symbol, ts: Type*): TBase = TBase(name, List(ts:_*))
  }

  case class TUniv(alpha: Symbol, bound: Type, tpe: Type) extends Type {
    override def ===(that: Type) = that match {
      case TUniv(beta, bound1, tpe1) => bound === bound1 && tpe === SubstType(beta, TVar(alpha))(tpe1)
      case _ => false
    }
  }
  object TUniv {
    def apply(alpha: Symbol, tpe: Type): TUniv = TUniv(alpha, Top, tpe)
  }
}
package djc.lang.typ

object Types {
  abstract class Type {
    /**
     * Equality modulo bound vars
     */
    def ===(that: Type): Boolean = this == that

    def <:<(that: Type): Boolean = this === that
    def <:<(that: Option[Type]): Boolean = that match {
      case None => true
      case Some(that) => this === that
    }
    def >:>(that: Type) = that <:< this
  }

  case object Unit extends Type

  case class TSvc(params: List[Type]) extends Type {
    override def ===(that: Type) = that match {
      case TSvc(params1) => params.corresponds(params1)(_ === _)
      case _ => false
    }

    override def <:<(that: Type) = that match {
      case TSvc(params1) => params.corresponds(params1)(_ >:> _) // contra-variant in arguments
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

    override def <:<(that: Type) = that match {
      case TSrvRep(svcs1) => svcs1 forall {
        case(s, tpe) => svcs.contains(s) && svcs(s) <:< tpe
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
    override def <:<(that: Type) = that match {
      case TSrv(rep2) => rep <:< rep2
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

  case class TUniv(alpha: Symbol, bound: Option[Type], tpe: Type) extends Type {
    override def ===(that: Type) = that match {
      case TUniv(beta, bound1, tpe1) => boundEq(bound, bound1) && tpe === SubstType(beta, TVar(alpha))(tpe1)
      case _ => false
    }

    override def <:<(that: Type) = that match {
      case TUniv(beta, bound1, tpe1) => boundSub(bound, bound1) && tpe <:< SubstType(beta, TVar(alpha))(tpe1)
      case _ => false
    }

    def boundEq(t1: Option[Type], t2: Option[Type]) = (t1, t2) match {
      case (None, None) => true
      case (Some(t1), Some(t2)) => t1 === t2
      case _ => false
    }

    // contra-variant (subtype accepts at least everything supertype accepts)
    //     If T1 <: T2  and  T2[U] well-typed,   then  T1[U] well-typed
    def boundSub(t1: Option[Type], t2: Option[Type]) = (t1, t2) match {
      case (None, _) => true
      case (Some(t1), Some(t2)) => t1 >:> t2
      case _ => false
    }
  }
  object TUniv {
    def apply(alpha: Symbol, tpe: Type): TUniv = TUniv(alpha, None, tpe)
    def apply(alpha: Symbol, bound: Type, tpe: Type): TUniv = TUniv(alpha, Some(bound), tpe)
  }
}
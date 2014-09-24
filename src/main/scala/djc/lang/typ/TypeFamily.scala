package djc.lang.typ

import util.ListOps._
import djc.lang.Gensym

trait TypeFamily  {
  self: TypeOps =>

  abstract class Type {
    /**
     * Equality modulo bound vars
     */
    def ===(that: Type): Boolean = this == that
    def toFamily(TF: TypeFamily): TF.Type
    val family: TypeFamily with TypeOps = self
  }

  case object Top extends Type {
    override def toFamily(TF: TypeFamily) = TF.Top
  }
  case object Bot extends Type {
    override def toFamily(TF: TypeFamily) = TF.Bot
  }
  
  case object Unit extends Type {
    override def toFamily(TF: TypeFamily) = TF.Unit
  }

  case class TSvc(params: List[Type]) extends Type {
    override def ===(that: Type) = that match {
      case TSvc(params1) => params.corresponds(params1)(_ === _)
      case _ => false
    }
    
    override def toFamily(TF: TypeFamily) = TF.TSvc(params map {_.toFamily(TF) } )
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
    
    override def toFamily(TF: TypeFamily) = TF.TSrvRep(svcs.mapValues {_.toFamily(TF)})
  }
  object TSrvRep {
    def apply(svcs: (Symbol, Type)*): TSrvRep = TSrvRep(Map(svcs: _*))
  }

  case class TSrv(rep: Type) extends Type {
    override def ===(that: Type) = that match {
      case TSrv(rep2) => rep === rep2
      case _ => false
    }
    
    override def toFamily(TF: TypeFamily) = TF.TSrv(rep.toFamily(TF))
  }


  case class TVar(alpha: Symbol) extends Type {
    override def toFamily(TF: TypeFamily) = TF.TVar(alpha)
  }

  case class TBase(name: Symbol, ts: List[Type]) extends Type {
    override def ===(that: Type) = that match {
      case TBase(`name`, ts1) =>
        ts.corresponds(ts1)(_ === _)
      case _ => false
    }
    
    override def toFamily(TF: TypeFamily) = TF.TBase(name, ts map {_.toFamily(TF)})
  }
  object TBase {
    def apply(name: Symbol, ts: Type*): TBase = TBase(name, List(ts:_*))
  }

  case class TUniv(alpha: Symbol, bound: Type, tpe: Type) extends Type {
    override def ===(that: Type) = that match {
      case TUniv(beta, bound1, tpe1) if bound === bound1 =>
        val (_, tpefixed, tpe1fixed) = captureAvoiding(alpha, tpe, beta, tpe1)
        tpefixed === tpe1fixed

      case _ => false
    }
    
    override def toFamily(TF: TypeFamily) = TF.TUniv(alpha, bound.toFamily(TF), tpe.toFamily(TF))
  }
  object TUniv {
    def apply(alpha: Symbol, tpe: Type): TUniv = TUniv(alpha, Top, tpe)
  }

  trait Mapper {
    final type TMapT = PartialFunction[Type,Type]

    def apply(tpe: Type): Type = mapType(tpe)

    def mapType: TMapT = {
      case Top => Top
      case Unit => Unit
      case Bot => Bot
      case TSvc(ts) => TSvc((ts map mapType))
      case TSrvRep(svcs) => TSrvRep(svcs mapValues mapType)
      case TSrv(t) => TSrv(mapType(t))
      case TVar(alpha) => TVar(alpha)
      case TBase(name, ts) => TBase(name, ts map mapType)
      case TUniv(alpha, bound, tpe1) => TUniv(alpha, mapType(bound), mapType(tpe1))
    }
  }

  trait StrictFold[T] {
    final type FoldT = PartialFunction[Type, T]

    def apply(t: Type): T

    def foldType(init: T): FoldT = {
      case Top =>
        init
      case Bot =>
        init
      case Unit =>
        init
      case TSvc(ts) =>
        ts.foldLeft(init)(foldType(_)(_))
      case TSrvRep(svcs) =>
        svcs.foldLeft(init) {
          (i, kv) => foldType(i)(kv._2)
        }
      case TSrv(t) => foldType(init)(t)
      case TVar(alpha) =>
        init
      case TBase(name, ts) =>
        ts.foldLeft(init)(foldType(_)(_))
      case TUniv(alpha, bound1, tpe1) =>
        foldType(foldType(init)(bound1))(tpe1)
    }
  }

  trait LazyFold[T] {
    final type FoldT = PartialFunction[Type, T]

    def apply(t: Type): T

    def foldType(init: => T): FoldT = {
      case Top =>
        init
      case Bot =>
        init
      case Unit =>
        init
      case TSvc(ts) =>
        ts.lazyFoldr(init)( (tpe, t) => foldType(t)(tpe) )
      case TSrvRep(svcs) =>
        svcs.toList.lazyFoldr(init) ( (p, t) => foldType(t)(p._2)  )
      case TSrv(t) => foldType(init)(t)
      case TVar(alpha) =>
        init
      case TBase(name, ts) =>
        ts.lazyFoldr(init)( (tpe, t) => foldType(t)(tpe) )
      case TUniv(alpha, bound1, tpe1) =>
        foldType(foldType(init)(tpe1))(bound1)
    }
  }

}

object Types extends TypeFamily with DefaultTypeOpsImpl

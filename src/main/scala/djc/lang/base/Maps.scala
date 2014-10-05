package djc.lang.base

import djc.lang.Syntax
import djc.lang.TypedLanguage._
import djc.lang.TypedLanguage.types._
import djc.lang.base.Bool._
import djc.lang.sem.SemanticException

object MapsOps {
  val TMap = TUniv('K, TUniv('V, TBase('Map, TVar('K), TVar('V))))

  case class MapValue(m: Map[Value, Value]) extends Value {
    def toExp = m.foldRight(Syntax.BaseCall(Empty.eraseType)) {
      case ((k, v), mape) => Syntax.BaseCall(Insert.eraseType, mape, k.toExp, v.toExp)
    }
  }

  case object Empty extends BaseOp('K << Top, 'V << Top)(Nil, TMap('K, 'V)) {
    def reduce(vs: List[Value]) = MapValue(Map())
  }


  case object Contains extends BaseOp('K << Top, 'V << Top)(TMap('K, 'V) :: TVar('K) :: Nil, TBool) {
    def reduce(vs: List[Value]) = vs match {
      case MapValue(m) :: vk :: Nil =>
        BoolValue(m.contains(vk))
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $vs")
    }
  }

  case object Get extends BaseOp('K << Top, 'V << Top)(TMap('K, 'V) :: TVar('K) :: Nil, 'V) {
    def reduce(vs: List[Value]) = vs match {
      case MapValue(m) :: vk :: Nil =>
        m(vk)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $vs")
    }
  }

  case object Remove extends BaseOp('K << Top, 'V << Top)(TMap('K, 'V) :: TVar('K) :: Nil, TMap('K, 'V)) {
    def reduce(vs: List[Value]) = vs match {
      case MapValue(m) :: vk :: Nil =>
        MapValue(m - vk)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $vs")
    }
  }

  case object Insert extends BaseOp('K << Top, 'V << Top)(TMap('K, 'V) :: TVar('K) :: TVar('V) :: Nil, TMap('K, 'V)) {
    def reduce(vs: List[Value]) = vs match {
      case MapValue(m) :: vk :: vv :: Nil =>
        MapValue(m + (vk -> vv))
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $vs")
    }
  }
}

object Maps {
  import MapsOps._

  val TMap = MapsOps.TMap

  def map(tk: Type, tv: Type)(es: (Exp, Exp)*): BaseCall =
    es.foldRight(empty(tk, tv)) { case ((k, v), mape) => mape.insert(k, v)}

  def empty(k: Type, v: Type) = BaseCall(Empty, List(k, v))

  implicit def infixSymbolMap(s: Symbol) = InfixExpMap(Var(s))
  implicit def infixMapExp(e: Exp) = InfixExpMap(e)
  case class InfixExpMap(e: Exp) {
    def m = this
    def insert(k: Type, v: Type, p: (Exp, Exp)): BaseCall = BaseCall(Insert, List(k,v), e, p._1, p._2)
    def hasKey(k: Type, v: Type, ke: Exp): BaseCall = BaseCall(Contains, List(k,v), e, ke)
    def get(k: Type, v: Type, ke: Exp): BaseCall = BaseCall(Get, List(k,v), e, ke)
    def remove(k: Type, v: Type, ke: Exp): BaseCall = BaseCall(Remove, List(k,v), e, ke)
  }

  implicit class BaseCallInfixMap(bc: BaseCall) {
    def insert(p: (Exp, Exp)): BaseCall = insert(p._1, p._2)
    def insert(e1: Exp, e2: Exp): BaseCall = bc.resultType match {
      case TBase('Map, tk :: tv :: Nil) => BaseCall(Insert, List(tk,tv), bc, e1, e2)
    }
    def hasKey(ke: Exp): BaseCall = bc.resultType match {
      case TBase('Map, tk :: tv :: Nil) => BaseCall(Contains, List(tk,tv), bc, ke)
    }
    def get(ke: Exp): BaseCall = bc.resultType match {
      case TBase('Map, tk :: tv :: Nil) => BaseCall(Get, List(tk,tv), bc, ke)
    }
    def remove(ke: Exp): BaseCall = bc.resultType match {
      case TBase('Map, tk :: tv :: Nil) => BaseCall(Remove, List(tk,tv), bc, ke)
    }
  }
}

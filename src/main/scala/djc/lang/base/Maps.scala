package djc.lang.base

import djc.lang.TypedSyntax._
import djc.lang.base.Bool._
import djc.lang.sem.SemanticException
import djc.lang.typ.Types._

object Maps {
  val TMap = TUniv('K, TUniv('V, TBase('Map, TVar('K), TVar('V))))

  case class MapValue(m: Map[Value, Value]) extends Value {
    def toExp = BaseCall(MapLit(Top,Top,m)).eraseType
  }

  case class MapLit(k: Type, v: Type, m: Map[Value, Value]) extends BaseOp('K << Top, 'V << Top)(Nil, TMap('K, 'V)) {
    def reduce(vs: List[Value]) = MapValue(m)
    override lazy val eraseType = MapLit(Top, Top, m)
  }

  def empty(k: Type, v: Type) = BaseCall(MapLit(k,v, Map()), List(k, v))

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


  implicit def infixSymbol(s: Symbol) =  InfixExp(Var(s))
  implicit def infixMapExp(e: Exp) = InfixExp(e)
  case class InfixExp(e: Exp) {
    def insert(k: Type, v: Type, p: (Exp, Exp)): Exp = BaseCall(Insert, List(k,v), e, p._1, p._2)
    def hasKey(k: Type, v: Type, ke: Exp): Exp = BaseCall(Contains, List(k,v), e, ke)
    def get(k: Type, v: Type, ke: Exp): Exp = BaseCall(Get, List(k,v), e, ke)
    def remove(k: Type, v: Type, ke: Exp): Exp = BaseCall(Remove, List(k,v), e, ke)
  }
}

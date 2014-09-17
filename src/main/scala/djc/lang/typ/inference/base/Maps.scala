package djc.lang.typ.inference.base

import djc.lang.TypedSyntax._
import djc.lang.base.MapsOps


object Maps {
  import MapsOps._

  val TMap = MapsOps.TMap

  def map(es: (Exp, Exp)*): BaseCall =
    es.foldRight(empty) { case ((k, v), mape) => BaseCall(Insert, Nil, mape, k, v)  }

  def empty = BaseCall(Empty, Nil, Nil)

  //TODO make this implicit value classes
  implicit def infixSymbolExtMap(s: Symbol) = InfixExpExtMap(Var(s))
  implicit def infixExtMapExp(e: Exp) = InfixExpExtMap(e)
  case class InfixExpExtMap(e: Exp) {
    def m = this
    def insert(p: (Exp, Exp)): BaseCall = BaseCall(Insert, Nil, e, p._1, p._2)
    def hasKey(ke: Exp): BaseCall = BaseCall(Contains, Nil, e, ke)
    def get(ke: Exp): BaseCall = BaseCall(Get, Nil, e, ke)
    def remove(ke: Exp): BaseCall = BaseCall(Remove, Nil, e, ke)
  }
}

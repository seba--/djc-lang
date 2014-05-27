package djc.lang.base

import djc.lang.TypedSyntax._
import djc.lang.typ.Types._
import djc.lang.sem.SemanticException

object Integer {

  val TInteger = TBase('Int)

  case class IntValue(i: Int) extends BaseValue {
    def toExp = BaseCall(IntLit(i), Nil).eraseType
  }

  case class IntLit(i: Int) extends BaseOp(Nil, TInteger) {
    def reduce(es: List[BaseValue]) = IntValue(i)
  }
  implicit def mkIntLit(n: Int): Exp = BaseCall(IntLit(n))

  case object Plus extends BaseOp(List(TInteger, TInteger), TInteger) {
    def reduce(es: List[BaseValue]) = es match {
      case IntValue(i1)::IntValue(i2)::Nil => IntValue(i1 + i2)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $es")
    }
  }

  case object Sub extends BaseOp(List(TInteger, TInteger), TInteger) {
    def reduce(es: List[BaseValue]) = es match {
      case IntValue(i1)::IntValue(i2)::Nil => IntValue(i1 - i2)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $es")
    }
  }

  case object Mul extends BaseOp(List(TInteger, TInteger), TInteger) {
    def reduce(es: List[BaseValue]) = es match {
      case IntValue(i1)::IntValue(i2)::Nil => IntValue(i1 * i2)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $es")
    }
  }

  case object Div extends BaseOp(List(TInteger, TInteger), TInteger) {
    def reduce(es: List[BaseValue]) = es match {
      case IntValue(i1)::IntValue(i2)::Nil => IntValue(i1 * i2)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $es")
    }
  }

}
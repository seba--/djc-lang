package djc.lang.base

import djc.lang.TypedLanguage._
import djc.lang.TypedLanguage.types._
import djc.lang.sem.SemanticException

object DoubleCompare {
  import Bool._
  import Double._

  case object Eq extends BaseOp(List(TDouble, TDouble), TBool) {
    def reduce(es: List[Value]) = es match {
      case DoubleValue(i1)::DoubleValue(i2)::Nil => BoolValue(i1 == i2)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $es")
    }
  }

  case object Lt extends BaseOp(List(TDouble, TDouble), TBool) {
    def reduce(es: List[Value]) = es match {
      case DoubleValue(i1)::DoubleValue(i2)::Nil => BoolValue(i1 < i2)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $es")
    }
  }

  case object Le extends BaseOp(List(TDouble, TDouble), TBool) {
    def reduce(es: List[Value]) = es match {
      case DoubleValue(i1)::DoubleValue(i2)::Nil => BoolValue(i1 <= i2)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $es")
    }
  }

  case object Gt extends BaseOp(List(TDouble, TDouble), TBool) {
    def reduce(es: List[Value]) = es match {
      case DoubleValue(i1)::DoubleValue(i2)::Nil => BoolValue(i1 > i2)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $es")
    }
  }

  case object Ge extends BaseOp(List(TDouble, TDouble), TBool) {
    def reduce(es: List[Value]) = es match {
      case DoubleValue(i1)::DoubleValue(i2)::Nil => BoolValue(i1 >= i2)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $es")
    }
  }

  implicit def infixExpIntegerCompareLit(e: Double) = InfixExp(e)
  implicit def infixExpDoubleCompareVar(e: Symbol) = InfixExp(Var(e))
  implicit def infixExpDoubleCompare(e: Exp) = InfixExp(e)
  case class InfixExp(e1: Exp) {
    def ===(e2: Exp) = BaseCall(Eq, e1, e2)
    def <(e2: Exp) = BaseCall(Lt, e1, e2)
    def <==(e2: Exp) = BaseCall(Le, e1, e2)
    def >(e2: Exp) = BaseCall(Gt, e1, e2)
    def >==(e2: Exp) = BaseCall(Ge, e1, e2)
  }
}
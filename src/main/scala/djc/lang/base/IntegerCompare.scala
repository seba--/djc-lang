package djc.lang.base

import djc.lang.TypedLanguage._
import djc.lang.TypedLanguage.types._
import djc.lang.sem.SemanticException

object IntegerCompare {
  import Bool._
  import Integer._

  case object Eq extends BaseOp(List(TInteger, TInteger), TBool) {
    def reduce(es: List[Value]) = es match {
      case IntValue(i1)::IntValue(i2)::Nil => BoolValue(i1 == i2)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $es")
    }
  }

  case object Neq extends BaseOp(List(TInteger, TInteger), TBool) {
    def reduce(es: List[Value]) = es match {
      case IntValue(i1)::IntValue(i2)::Nil => BoolValue(i1 != i2)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $es")
    }
  }

  case object Lt extends BaseOp(List(TInteger, TInteger), TBool) {
    def reduce(es: List[Value]) = es match {
      case IntValue(i1)::IntValue(i2)::Nil => BoolValue(i1 < i2)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $es")
    }
  }

  case object Le extends BaseOp(List(TInteger, TInteger), TBool) {
    def reduce(es: List[Value]) = es match {
      case IntValue(i1)::IntValue(i2)::Nil => BoolValue(i1 <= i2)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $es")
    }
  }

  case object Gt extends BaseOp(List(TInteger, TInteger), TBool) {
    def reduce(es: List[Value]) = es match {
      case IntValue(i1)::IntValue(i2)::Nil => BoolValue(i1 > i2)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $es")
    }
  }

  case object Ge extends BaseOp(List(TInteger, TInteger), TBool) {
    def reduce(es: List[Value]) = es match {
      case IntValue(i1)::IntValue(i2)::Nil => BoolValue(i1 >= i2)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $es")
    }
  }

  implicit def infixExpIntegerCompareLit(e: Int) = InfixExp(e)
  implicit def infixExpIntegerCompareVar(e: Symbol) = InfixExp(Var(e))
  implicit def infixExpIntegerCompare(e: Exp) = InfixExp(e)
  case class InfixExp(e1: Exp) {
    def <>(e2: Exp) = BaseCall(Neq, e1, e2)
    def ===(e2: Exp) = BaseCall(Eq, e1, e2)
    def <(e2: Exp) = BaseCall(Lt, e1, e2)
    def <==(e2: Exp) = BaseCall(Le, e1, e2)
    def >(e2: Exp) = BaseCall(Gt, e1, e2)
    def >==(e2: Exp) = BaseCall(Ge, e1, e2)
  }
}
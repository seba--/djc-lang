package djc.lang.base

import djc.lang.TypedSyntax._
import djc.lang.sem.SemanticException

object IntegerCompare {
  import Bool._
  import Integer._

  case object Eq extends BaseOp(List(TInteger, TInteger), TBool) {
    def reduce(es: List[BaseValue]) = es match {
      case IntValue(i1)::IntValue(i2)::Nil => BoolValue(i1 == i2)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $es")
    }
  }

  case object Lt extends BaseOp(List(TInteger, TInteger), TBool) {
    def reduce(es: List[BaseValue]) = es match {
      case IntValue(i1)::IntValue(i2)::Nil => BoolValue(i1 < i2)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $es")
    }
  }

  case object Le extends BaseOp(List(TInteger, TInteger), TBool) {
    def reduce(es: List[BaseValue]) = es match {
      case IntValue(i1)::IntValue(i2)::Nil => BoolValue(i1 <= i2)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $es")
    }
  }

  case object Gt extends BaseOp(List(TInteger, TInteger), TBool) {
    def reduce(es: List[BaseValue]) = es match {
      case IntValue(i1)::IntValue(i2)::Nil => BoolValue(i1 > i2)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $es")
    }
  }

  case object Ge extends BaseOp(List(TInteger, TInteger), TBool) {
    def reduce(es: List[BaseValue]) = es match {
      case IntValue(i1)::IntValue(i2)::Nil => BoolValue(i1 >= i2)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $es")
    }
  }
}
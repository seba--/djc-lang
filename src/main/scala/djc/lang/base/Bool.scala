package djc.lang.base

import djc.lang.TypedSyntax._
import djc.lang.TypedSyntaxDerived._
import djc.lang.typ.Types._
import djc.lang.sem.SemanticException

object Bool {

  val TBool = TBase('Bool)

  case class BoolValue(b: Boolean) extends BaseValue {
    def toExp =
      if (b)
        BaseCall(True, Nil).eraseType
      else
        BaseCall(False, Nil).eraseType
  }

  case object True extends BaseOp(Nil, TBool) {
    def reduce(es: List[BaseValue]) = BoolValue(true)
  }

  case object False extends BaseOp(Nil, TBool) {
    def reduce(es: List[BaseValue]) = BoolValue(false)
  }

  case object And extends BaseOp(List(TBool, TBool), TBool) {
    def reduce(es: List[BaseValue]) = es match {
      case BoolValue(b1)::BoolValue(b2)::Nil => BoolValue(b1 && b2)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $es")
    }
  }

  case object Or extends BaseOp(List(TBool, TBool), TBool) {
    def reduce(es: List[BaseValue]) = es match {
      case BoolValue(b1)::BoolValue(b2)::Nil => BoolValue(b1 || b2)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $es")
    }
  }

  case object Not extends BaseOp(List(TBool), TBool) {
    def reduce(es: List[BaseValue]) = es match {
      case BoolValue(b)::Nil => BoolValue(!b)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $es")
    }
  }

  case object If extends BaseOp(List(TBool, TThunk, TThunk), TThunk) {
    def reduce(es: List[BaseValue]) = es(0) match {
      case BoolValue(b) =>
        if (b)
          es(1)
        else
          es(2)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $es")
    }
  }

  def tru = BaseCall(True, Nil)
  def fal = BaseCall(False, Nil)
  implicit def infixExpBoolVar(e: Symbol) = InfixExp(Var(e))
  implicit def infixExpBool(e: Exp) = InfixExp(e)
  case class InfixExp(e1: Exp) {
    def &&(e2: Exp) = BaseCall(And, e1, e2)
    def ||(e2: Exp) = BaseCall(Or, e1, e2)
  }
  implicit def !(e: Exp) = BaseCall(Not, e)
}
package djc.lang.base

import djc.lang.TypedSyntax._
import djc.lang.typ.Types._
import djc.lang.sem.SemanticException

object Integer {

  val TInteger = TBase('Int)

  case class IntValue(i: Int) extends Value {
    def toExp = BaseCall(IntLit(i)).eraseType
  }
  implicit def mkIntValue(i: Int): Value = IntValue(i)

  case class IntLit(i: Int) extends BaseOp(Nil, TInteger) {
    def reduce(es: List[Value]) = IntValue(i)
  }
  implicit def mkIntLit(n: Int): Exp = BaseCall(IntLit(n))
  implicit def mkIntLitPair(p: (Int, Type)): (Exp, Type) = (p._1, p._2)
  implicit def mkIntList(ints: List[Int]): List[Exp] = ints map mkIntLit

  case object Plus extends BaseOp(List(TInteger, TInteger), TInteger) {
    def reduce(es: List[Value]) = es match {
      case IntValue(i1)::IntValue(i2)::Nil => IntValue(i1 + i2)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $es")
    }
  }

  case object Sub extends BaseOp(List(TInteger, TInteger), TInteger) {
    def reduce(es: List[Value]) = es match {
      case IntValue(i1)::IntValue(i2)::Nil => IntValue(i1 - i2)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $es")
    }
  }

  case object Mul extends BaseOp(List(TInteger, TInteger), TInteger) {
    def reduce(es: List[Value]) = es match {
      case IntValue(i1)::IntValue(i2)::Nil => IntValue(i1 * i2)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $es")
    }
  }

  case object Div extends BaseOp(List(TInteger, TInteger), TInteger) {
    def reduce(es: List[Value]) = es match {
      case IntValue(i1)::IntValue(i2)::Nil => IntValue(i1 / i2)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $es")
    }
  }

  case object Mod extends BaseOp(List(TInteger, TInteger), TInteger) {
    def reduce(es: List[Value]) = es match {
      case IntValue(i1)::IntValue(i2)::Nil => IntValue(i1 % i2)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $es")
    }
  }

  implicit def infixExpIntegerVar(e: Symbol) = InfixExp(Var(e))
  implicit def infixExpInteger(e: Exp) = InfixExp(e)
  case class InfixExp(e1: Exp) {
    def i = this
    def +(e2: Exp) = BaseCall(Plus, e1, e2)
    def +(e2: Int) = BaseCall(Plus, e1, e2)
    def +(e2: Symbol) = BaseCall(Plus, e1, e2)
    def -(e2: Exp) = BaseCall(Sub, e1, e2)
    def -(e2: Int) = BaseCall(Sub, e1, e2)
    def /(e2: Exp) = BaseCall(Div, e1, e2)
    def *(e2: Exp) = BaseCall(Mul, e1, e2)
    def mod(e2: Exp) = BaseCall(Mod, e1, e1)
  }

}
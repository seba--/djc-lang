package djc.lang.base

import djc.lang.TypedSyntax._
import djc.lang.typ.Types._
import djc.lang.sem.SemanticException

object String {

  val TString = TBase('String)

  case class StringValue(i: String) extends Value {
    def toExp = BaseCall(StringLit(i), Nil).eraseType
  }

  case class StringLit(i: String) extends BaseOp(Nil, TString) {
    def reduce(es: List[Value]) = StringValue(i)
  }
  implicit def mkStringLit(n: String): Exp = BaseCall(StringLit(n))

  case object Plus extends BaseOp(List(TString, TString), TString) {
    def reduce(es: List[Value]) = es match {
      case StringValue(i1)::StringValue(i2)::Nil => StringValue(i1 + i2)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $es")
    }
  }

  case object Print extends BaseOp(List(TString, ?()), Unit) {
    def reduce(es: List[Value]) = es match {
      case StringValue(s)::k::Nil => {
        println(s"${System.currentTimeMillis()}: $s")
        k.toExp!!()
      }
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $es")
    }
  }
  def print(msg: Exp, k: Exp) = BaseCall(Print, List(msg, k))

  case object AsString extends BaseOp(List(Top), TString) {
    def reduce(es: List[Value]) = es match {
      case v::Nil => StringValue(v.toString)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $es")
    }
  }
  def asString(a: Exp) = BaseCall(AsString, List(a))


  implicit def infixExpIntegerVar(e: Symbol) = InfixExp(Var(e))
  implicit def infixExpIntegerLit(e: String) = InfixExp(e)
  implicit def infixExpInteger(e: Exp) = InfixExp(e)
  case class InfixExp(e1: Exp) {
    def +(e2: Exp) = BaseCall(Plus, e1, e2)
    def +(e2: String) = BaseCall(Plus, e1, e2)
    def asString = BaseCall(AsString, e1)
  }

}
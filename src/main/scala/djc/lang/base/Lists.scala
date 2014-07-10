package djc.lang.base

import djc.lang.TypedSyntax._
import djc.lang.base.Integer._
import djc.lang.sem.SemanticException
import djc.lang.typ.Types._


object Lists {

  val TList = TUniv('alpha, TBase('List, TVar('alpha)))

  case class ListValue(t: Type, vs: List[Value]) extends Value {
    def toExp = toTypedExp.eraseType
    def toTypedExp = BaseCall(ListLit(t, vs), t::Nil)
  }
  object ListValue {
    def apply(t: Type, vs: Value*): ListValue = ListValue(t, List(vs:_*))
  }

  case class ListLit(t: Type, vs: List[Value]) extends BaseOp(('alpha,None)::Nil, Nil, TList('alpha)) {
    def reduce(vs: List[Value]) = ListValue(t, vs)
  }
  object ListLit {
    def apply(t: Type, vs: Value*): ListLit = ListLit(t, List(vs:_*))
  }

  def LIST(t: Type, vs: Value*): Exp = ListValue(t, vs:_*).toTypedExp

  def NIL(t: Type) = BaseCall(ListLit(t, Nil), List(t))

  case object Cons extends BaseOp(('alpha,None)::Nil, TVar('alpha)::TList('alpha)::Nil, TList('alpha)) {
    def reduce(vs: List[Value]) = vs match {
      case v :: ListValue(t, vs1) :: Nil => ListValue(t, v :: vs1)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $vs")
    }
  }

  case object ElementAt extends BaseOp(('alpha, None)::Nil, TInteger::TList('alpha)::Nil, 'alpha) {
    def reduce(es: List[Value]) = es match {
      case IntValue(i)::ListValue(t, vs)::Nil => vs(i)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $es")
    }
  }

  case object Plus extends BaseOp(('alpha, None)::Nil, TList('alpha)::TList('alpha)::Nil, TList('alpha)) {
    def reduce(es: List[Value]) = es match {
      case ListValue(t, vs1)::ListValue(_, vs2)::Nil => ListValue(t, vs1 ++ vs2)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $es")
    }
  }

  implicit def infixExpListVar(e: Symbol) = InfixExp(Var(e))
  implicit def infixExpList(e: Exp) = InfixExp(e)
  case class InfixExp(e1: Exp) {
    def i = this
    def ::(p: (Exp,Type)) = {
      val (e2,t) = p
      BaseCall(Cons, List(t), e2, e1)
    }
    def elemAt(t: Type)(e2: Exp) = BaseCall(ElementAt, List(t), e2, e1)
    def ++(t: Type)(e2: Exp) = BaseCall(Plus, List(t), e1, e2)
  }
}

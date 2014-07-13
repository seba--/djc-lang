package djc.lang.base

import djc.lang.TypedSyntax._
import djc.lang.base.Bool._
import djc.lang.base.Integer._
import djc.lang.sem.SemanticException
import djc.lang.typ.Types._


object Lists {

  val TList = TUniv('alpha, TBase('List, TVar('alpha)))

  case class ListValue(vs: List[Value]) extends Value {
    def toExp = BaseCall(ListLit(Top, vs)).eraseType
  }
  object ListValue {
    def apply(vs: Value*): ListValue = ListValue(List(vs:_*))
  }

  case class ListLit(t: Type, vs: List[Value]) extends BaseOp('alpha << Top)(Nil, TList('alpha)) {
    def reduce(vs1: List[Value]) = ListValue(vs)
    override lazy val eraseType = ListLit(Top, vs)
  }
  object ListLit {
    def apply(t: Type, vs: Value*): ListLit = ListLit(t, List(vs:_*))
  }

  def lst(t: Type, vs: Value*): Exp = BaseCall(ListLit(t, List(vs:_*)), List(t))
  def nil(t: Type) = BaseCall(ListLit(t, Nil), List(t))

  case object Cons extends BaseOp('alpha << Top)(TVar('alpha)::TList('alpha)::Nil, TList('alpha)) {
    def reduce(vs: List[Value]) = vs match {
      case v :: ListValue(vs1) :: Nil => ListValue(v :: vs1)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $vs")
    }
  }

  case object ElementAt extends BaseOp('alpha << Top)(TInteger::TList('alpha)::Nil, 'alpha) {
    def reduce(es: List[Value]) = es match {
      case IntValue(i)::ListValue(vs)::Nil => vs(i)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $es")
    }
  }

  case object Tail extends BaseOp('alpha << Top)(TList('alpha)::Nil, TList('alpha)) {
    def reduce(vs: List[Value]) = vs match {
      case ListValue(x :: xs) :: Nil => ListValue(xs)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $vs")
    }
  }

  case object Concat extends BaseOp('alpha << Top)(TList('alpha)::TList('alpha)::Nil, TList('alpha)) {
    def reduce(es: List[Value]) = es match {
      case ListValue(vs1)::ListValue(vs2)::Nil =>
        ListValue(vs1 ::: vs2)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $es")
    }
  }

  case object IsEmpty extends BaseOp('alpha << Top)(TList('alpha)::Nil, TBool) {
    def reduce(vs: List[Value]) = vs match {
      case ListValue(vs1) :: Nil => BoolValue(vs1.isEmpty)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $vs")
    }
  }

  case object Length extends BaseOp('alpha << Top)(TList('alpha)::Nil, TInteger) {
    def reduce(vs: List[Value]) = vs match {
      case ListValue(vs1) :: Nil => IntValue(vs1.length)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $vs")
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
    def isNil(t: Type) = BaseCall(IsEmpty, List(t), e1)
    def +++(t: Type, e2: Exp) = BaseCall(Concat, List(t), e1, e2)
    def head(t: Type) = elemAt(t)(0)
    def tail(t: Type) = BaseCall(Tail, List(t), e1)
    def length(t: Type) = BaseCall(Length, List(t), e1)
  }
}

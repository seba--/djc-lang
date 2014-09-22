package djc.lang.base

import djc.lang.Syntax
import djc.lang.TypedLanguage._
import djc.lang.base.Bool._
import djc.lang.base.Integer._
import djc.lang.sem.SemanticException


object ListsOps {

  val TList = TUniv('alpha, TBase('List, TVar('alpha)))

  case class ListValue(vs: List[Value]) extends Value {
    def toExp =  {
      vs.foldRight(Syntax.BaseCall(Empty.eraseType)) {
        case (v, le) => Syntax.BaseCall(Cons.eraseType, v.toExp, le)
      }
    }
  }
  object ListValue {
    def apply(vs: Value*): ListValue = ListValue(List(vs:_*))
  }

  case object Empty extends BaseOp('alpha << Top)(Nil, TList('alpha)) {
    def reduce(vs: List[Value]) = ListValue(Nil)
  }

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
}

object Lists {
  import ListsOps._

  val TList = ListsOps.TList

  def lst(t: Type, es: Exp*): BaseCall =
    es.foldRight(nil(t)) { case (e, liste) => BaseCall(Cons, List(t), e, liste) }
  def nil(t: Type) = BaseCall(Empty, List(t))

  implicit def infixExpListVar(e: Symbol) = InfixExpList(Var(e))
  implicit def infixExpList(e: Exp) = InfixExpList(e)
  case class InfixExpList(e1: Exp) {
    def l = this
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

  implicit class BaseCallInfixList(bc: BaseCall) {
    def ::(e2: BaseCall): BaseCall = bc.resultType match {
      case TBase('List, t :: Nil) => BaseCall(Cons, List(t), e2, bc)
    }
    def elemAt(e2: BaseCall) = bc.resultType match {
      case TBase('List, t :: Nil) => BaseCall(ElementAt, List(t), e2, bc)
    }
    def isNil = bc.resultType match {
      case TBase('List, t :: Nil) => BaseCall(IsEmpty, List(t), bc)
    }
    def +++(e2: BaseCall) = bc.resultType match {
      case TBase('List, t :: Nil) => BaseCall(Concat, List(t), bc, e2)
    }
    def head = elemAt(mkIntLit(0))
    def tail = bc.resultType match {
      case TBase('List, t :: Nil) => BaseCall(Tail, List(t), bc)
    }
    def length = bc.resultType match {
      case TBase('List, t :: Nil) => BaseCall(Length, List(t), bc)
    }
  }
}

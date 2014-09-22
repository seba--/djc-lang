package djc.lang.typ.inference.base

import djc.lang.TypedLanguage._
import djc.lang.base.Integer._
import djc.lang.base.ListsOps

/**
 *  List sugar without type applications (which should be inferred)
 */
object Lists {
  import djc.lang.base.ListsOps._

  val TList = ListsOps.TList

  def lst(es: Exp*): BaseCall =
    es.foldRight(nil) { case (e, liste) => BaseCall(Cons, Nil, e, liste) }
  def nil = BaseCall(Empty, Nil, Nil)

  //TODO make this implicit value classes
  implicit def infixExpExtListVar(e: Symbol) = InfixExtExpList(Var(e))
  implicit def infixExpExtList(e: Exp) = InfixExtExpList(e)
  case class InfixExtExpList(e1: Exp) {
    def l = this
    def ::(e: Exp) = {
      BaseCall(Cons, Nil, e, e1)
    }
    def elemAt(e2: Exp) = BaseCall(ElementAt, Nil, e2, e1)
    def isNil = BaseCall(IsEmpty, Nil, e1)
    def +++(e2: Exp) = BaseCall(Concat, Nil, e1, e2)
    def head = elemAt(0)
    def tail = BaseCall(Tail, Nil, e1)
    def length = BaseCall(Length, Nil, e1)
  }
}

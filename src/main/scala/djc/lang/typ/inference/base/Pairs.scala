package djc.lang.typ.inference.base

import djc.lang.TypedLanguage.{Var, BaseCall, Exp}
import djc.lang.base.PairsOps

import scala.annotation.tailrec

object Pairs {
  import PairsOps._

  val TPair = PairsOps.TPair

  def pair(p1: Exp, p2: Exp, ps: Exp*): Exp = {
    val pall = p1 +: p2 +: ps
    pall.dropRight(1).foldRight(pall.last) {
      (e, ep) => BaseCall(Pair, Nil, e, ep)
    }
  }

  implicit def infixExpExtPairVar(e: Symbol) = InfixExpExtPair(Var(e))
  implicit def infixExpExtPair(e: Exp) = InfixExpExtPair(e)
  case class InfixExpExtPair(e1: Exp) {
    def p = this
    def fst = ith(e1, 1)
    def snd = ith(e1, 2)
    def thrd = ith(e1, 3)
    def frth = ith(e1, 4)
  }

  @tailrec
  def ith(e: Exp, n: Int): Exp = n match {
    case 1 =>
      BaseCall(Fst, Nil, e)

    case i if i > 1 =>
      ith(BaseCall(Snd, Nil, e), i - 1)
  }
}

package djc.lang.base

import djc.lang.TypedSyntax._
import djc.lang.base.Bool._
import djc.lang.sem.SemanticException
import djc.lang.typ.Types._

object Pairs {

  val TPair = TUniv('A, TUniv('B, TBase('Pair, 'A, 'B)))


  case class PairVal(fst: Value, snd: Value) extends Value {
    def toExp = BaseCall(PairLit(fst, Top, snd, Top)).eraseType
  }

  case class PairLit(fst: Value, fstT: Type, snd: Value, sndT: Type) extends BaseOp(Nil, TPair(fstT,sndT)) {
    def reduce(vs: List[Value]) = PairVal(fst, snd)
  }


  implicit def mkPair(p: (Value, Type, Value, Type)): Exp = {
    val (v1,t1,v2,t2) = p
    BaseCall(PairLit(v1,t1,v2,t2))
  }

  case object Fst extends BaseOp('A << None, 'B << None)(List(TPair('A,'B)), 'A) {
    def reduce(vs: List[Value]) = vs match {
      case PairVal(fst,_) :: Nil => fst
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $vs")
    }
  }

  case object Snd extends BaseOp('A << None, 'B << None)(List(TPair('A,'B)), 'B) {
    def reduce(vs: List[Value]) = vs match {
      case PairVal(_,snd) :: Nil => snd
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $vs")
    }
  }

  implicit def infixExpPairVar(e: Symbol) = InfixExp(Var(e))
  implicit def infixExpPair(e: Exp) = InfixExp(e)
  case class InfixExp(e1: Exp) {
    def i = this
    def fst(t1: Type, t2: Type) = BaseCall(Fst, List(t1,t2), e1)
    def and(t1: Type, t2: Type) = BaseCall(Snd, List(t1,t2), e1)
  }
}

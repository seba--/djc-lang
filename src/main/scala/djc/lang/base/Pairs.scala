package djc.lang.base

import djc.lang.Syntax
import djc.lang.TypedSyntax._
import djc.lang.base.Bool._
import djc.lang.sem.SemanticException
import djc.lang.typ.Types._

object Pairs {

  val TPair = TUniv('A, TUniv('B, TBase('Pair, 'A, 'B)))

  def TTuple(t1: Type, t2: Type, ts: Type*) = {
    val tall = t1 +: t2 +: ts
    tall.reduceRight( (t1, t2) => TPair(t1, t2)   )
  }

  case class PairVal(fst: Value, snd: Value) extends Value {
    def toExp = Syntax.BaseCall(Pair.eraseType, fst.toExp, snd.toExp)
  }

  case object Pair extends BaseOp('A << None, 'B << None)(List('A,'B), TPair('A,'B)) {
    def reduce(vs: List[Value]) = vs match {
      case v1 :: v2 :: Nil => PairVal(v1, v2)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $vs")
    }
  }

  def pair(p1: (Exp, Type), p2: (Exp, Type), ps: (Exp, Type)*): Exp = {
      val pall = p1 +: p2 +: ps
      pall.dropRight(1).foldRight(pall.last) {
        case ((e,t), (ep, tp)) => (BaseCall(Pair, List(t, tp), e, ep), TPair(t, tp))
      }._1
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

  case object Thrd extends BaseOp('A << None, 'B << None, 'C << None)(List(TPair('A, TPair('B, 'C))), 'C) {
    def reduce(vs: List[Value]) = vs match {
      case PairVal(_, PairVal(_, thrd)) :: Nil => thrd
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $vs")
    }
  }

  case object Frth extends BaseOp('A << None, 'B << None, 'C << None, 'D << None)(List(TPair('A, TPair('B, TPair('C, 'D)))), 'D) {
    def reduce(vs: List[Value]) = vs match {
      case PairVal(_, PairVal(_, PairVal(_, frth))) :: Nil => frth
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $vs")
    }
  }



  implicit def infixExpPairVar(e: Symbol) = InfixExp(Var(e))
  implicit def infixExpPair(e: Exp) = InfixExp(e)
  case class InfixExp(e1: Exp) {
    def i = this
    def fst(t1: Type, t2: Type) = BaseCall(Fst, List(t1,t2), e1)
    def snd(t1: Type, t2: Type) = BaseCall(Snd, List(t1,t2), e1)
    def thrd(t1: Type, t2: Type, t3: Type) = BaseCall(Thrd, List(t1,t2,t3), e1)
    def frth(t1: Type, t2: Type, t3: Type, t4: Type) = BaseCall(Frth, List(t1,t2,t3,t4), e1)
  }

  def fst(t: Type, e: Exp) = t match {
    case TBase('Pair, t1 :: t2 :: Nil) => e.fst(t1,t2)
  }
  def snd(t: Type, e: Exp) = t match {
    case TBase('Pair, t1 :: t2 :: Nil) => e.snd(t1,t2)
  }
  def thrd(t: Type, e: Exp) = t match {
    case TBase('Pair, t1 :: TBase('Pair, t2 :: t3 :: Nil) :: Nil) => e.thrd(t1,t2,t3)
  }
  def frth(t: Type, e: Exp) = t match {
    case TBase('Pair, t1 :: TBase('Pair, t2 :: TBase('Pair, t3 :: t4 :: Nil) :: Nil) :: Nil) => e.frth(t1,t2,t3,t4)
  }
}

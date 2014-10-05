package djc.lang.base

import djc.lang.Syntax
import djc.lang.TypedLanguage._
import djc.lang.TypedLanguage.types._
import djc.lang.sem.SemanticException

object PairsOps {

  val TPair = TUniv('A, TUniv('B, TBase('Pair, 'A, 'B)))

  object TTuple {
    def apply(t1: Type, t2: Type, ts: Type*) = {
      val tall = t1 +: t2 +: ts
      tall.reduceRight( (t1, t2) => TPair(t1, t2)   )
    }

    def unapplySeq(t: Type): Option[Seq[Type]] = t match {
      case TBase('Pair, t1 :: t2 :: Nil) =>
        t2 match {
          case TBase('Pair, t3 :: t4 :: Nil) =>
            Some(t1 +: t3 +: unapplySeq(t4).getOrElse(Seq(t4)))
          case _ => Some(Seq(t1,t2))
        }
      case _ => None
    }
  }

  case class PairVal(fst: Value, snd: Value) extends Value {
    def toExp = Syntax.BaseCall(Pair.eraseType, fst.toExp, snd.toExp)
  }

  case object Pair extends BaseOp('A << Top, 'B << Top)(List('A,'B), TPair('A,'B)) {
    def reduce(vs: List[Value]) = vs match {
      case v1 :: v2 :: Nil => PairVal(v1, v2)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $vs")
    }
  }


  case object Fst extends BaseOp('A << Top, 'B << Top)(List(TPair('A,'B)), 'A) {
    def reduce(vs: List[Value]) = vs match {
      case PairVal(fst,_) :: Nil => fst
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $vs")
    }
  }

  case object Snd extends BaseOp('A << Top, 'B << Top)(List(TPair('A,'B)), 'B) {
    def reduce(vs: List[Value]) = vs match {
      case PairVal(_,snd) :: Nil => snd
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $vs")
    }
  }
}

object Pairs {
  import PairsOps._

  val TPair = PairsOps.TPair
  val TTuple = PairsOps.TTuple

  def pair(p1: (Exp, Type), p2: (Exp, Type), ps: (Exp, Type)*): BaseCall = {
    val pall = p1 +: p2 +: ps
    pall.dropRight(1).foldRight(pall.last) {
      case ((e,t), (ep, tp)) => (BaseCall(Pair, List(t, tp), e, ep), TPair(t, tp))
    }._1.asInstanceOf[BaseCall]
  }

  def pair(t: Type, es: Exp*): BaseCall = (t,es) match {
    case (TTuple(t1,t2,ts@_*), Seq(e1,e2,es@_*))
      if (es.length == ts.length) =>
      pair(e1 -> t1, e2 -> t2, (es zip ts):_*)

    case _ => throw SemanticException(s"need a tuple type and matching arguments, got $t\n$es")
  }

  def pair(b1: BaseCall, b2: BaseCall, bs: BaseCall*): BaseCall = {
    val ball = b1 +: b2 +: bs
    ball.dropRight(1).foldRight(ball.last) {
      case (bc, pairbc) => BaseCall(Pair, List(bc.resultType, pairbc.resultType), bc, pairbc)
    }
  }

  implicit def infixExpPairVar(e: Symbol) = InfixExpPair(Var(e))
  implicit def infixExpPair(e: Exp) = InfixExpPair(e)
  case class InfixExpPair(e1: Exp) {
    def p = this
    def fst(t: Type) = Pairs.fst(t, e1)
    def snd(t: Type) = Pairs.snd(t, e1)
    def thrd(t: Type) = Pairs.thrd(t, e1)
    def frth(t: Type) = Pairs.frth(t, e1)
  }
  implicit class InfixBasecallPair(bc: BaseCall) {
    def fst = Pairs.fst(bc.resultType, bc)
    def snd = Pairs.snd(bc.resultType, bc)
    def thrd = Pairs.thrd(bc.resultType, bc)
    def frth = Pairs.frth(bc.resultType, bc)
  }

  def fst(t: Type, e: Exp) = ith(t,e,1)
  def snd(t: Type, e: Exp) = ith(t,e,2)
  def thrd(t: Type, e: Exp) = ith(t,e,3)
  def frth(t: Type, e: Exp) = ith(t,e,4)


  def ith(t: Type, e: Exp, n: Int): Exp = n match {
    case 1 => t match {
      case TBase('Pair, t1 :: t2 :: Nil) => BaseCall(Fst, List(t1,t2), e)
      case _ => e
    }

    case i if i > 1 => t match {
      case TBase('Pair, t1 :: t2 :: Nil) => ith(t2, BaseCall(Snd, List(t1,t2), e), i - 1)
    }
  }
}

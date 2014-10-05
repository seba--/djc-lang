package djc.lang.typ.inference.base

import djc.lang.Syntax
import djc.lang.TypedLanguage._
import djc.lang.TypedLanguage.types._
import djc.lang.sem.SemanticException

object PairsOps {

  case class PairVal(values: Value*) extends Value {
    if (values.length < 2) throw SemanticException("Pairs must have length of at least 2")
    def toExp = Syntax.BaseCall(Pair(values.length).eraseType, (values.map(_.toExp)):_*)
  }


  def nXBounds(n: Int): Seq[(Symbol, Type)] = nXs(n) zip Seq.fill(n)(Top)
  def nXTVars(n: Int): Seq[TVar] = nXs(n).map(TVar(_))

  case class Pair(n: Int) extends BaseOp(nXBounds(n):_*)(List(nXTVars(n):_*), TPair(nXTVars(n):_*)) {
    if (n < 2) throw SemanticException("Pairs must have length of at least 2")
    def reduce(vs: List[Value]) = vs.length match {
      case i if i == n => PairVal(vs.toSeq:_*)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $vs")
    }
  }

  case object Fst extends BaseOp('A << Top, 'B << Top)(List(TPair('A,'B)), 'A) {
    def reduce(vs: List[Value]) = vs match {
      case List(PairVal(v, _*)) => v
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $vs")
    }
  }

  case class Index(n: Int) extends BaseOp(nXBounds(n):_*)(List(TPair(nXTVars(n):_*)), TVar(Symbol(s"X$n"))) {
    if (n < 2) throw SemanticException("Index requires a length of at least 2")
    def reduce(vs: List[Syntax.Value]) = vs match {
      case List(PairVal(vs@_*)) if vs.length >= n =>
        vs(n)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $vs")
    }
  }
}


object Pairs {
  import PairsOps._

  def pair(p1: Exp, p2: Exp, ps: Exp*): Exp = {
    val pall = p1 +: p2 +: ps
    BaseCall(Pair(pall.length), Nil, pall:_*)
  }

  implicit def infixExpExtPairVar(e: Symbol) = new InfixExpExtPair(Var(e))
 // implicit def infixExpExtPair(e: Exp) = InfixExpExtPair(e)
  implicit class InfixExpExtPair(val e1: Exp) extends AnyVal {
    def p = this
    def fst = BaseCall(Fst, Nil, e1)
    def snd = BaseCall(Index(2), Nil, e1)
    def index(i: Int) = BaseCall(Index(i), Nil, e1)
    def thrd = BaseCall(Index(3), Nil, e1)
    def frth = BaseCall(Index(4), Nil, e1)
  }
}

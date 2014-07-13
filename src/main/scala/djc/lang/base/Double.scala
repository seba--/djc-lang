package djc.lang.base

import djc.lang.TypedSyntax._
import djc.lang.typ.Types._
import djc.lang.sem.SemanticException
import djc.lang.base.Integer.IntValue

object Double {

  val TDouble = TBase('Double)

  case class DoubleValue(i: Double) extends Value {
    def toExp = BaseCall(DoubleLit(i)).eraseType
  }
  implicit def mkDoubleValue(i: Double): Value = DoubleValue(i)

  case class DoubleLit(i: Double) extends BaseOp(Nil, TDouble) {
    def reduce(es: List[Value]) = DoubleValue(i)
  }
  implicit def mkDoubleLit(n: Double): BaseCall = BaseCall(DoubleLit(n))
  implicit def mkDoubleListP(p: (Double, Type)): (Exp, Type) = (p._1, p._2)

  case object Plus extends BaseOp(List(TDouble, TDouble), TDouble) {
    def reduce(es: List[Value]) = es match {
      case DoubleValue(i1)::DoubleValue(i2)::Nil => DoubleValue(i1 + i2)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $es")
    }
  }

  case object Sub extends BaseOp(List(TDouble, TDouble), TDouble) {
    def reduce(es: List[Value]) = es match {
      case DoubleValue(i1)::DoubleValue(i2)::Nil => DoubleValue(i1 - i2)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $es")
    }
  }

  case object Mul extends BaseOp(List(TDouble, TDouble), TDouble) {
    def reduce(es: List[Value]) = es match {
      case DoubleValue(i1)::DoubleValue(i2)::Nil => DoubleValue(i1 * i2)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $es")
    }
  }

  case object Div extends BaseOp(List(TDouble, TDouble), TDouble) {
    def reduce(es: List[Value]) = es match {
      case DoubleValue(i1)::DoubleValue(i2)::Nil => DoubleValue(i1 / i2)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $es")
    }
  }

  case object Pow extends BaseOp(List(TDouble, TDouble), TDouble) {
    def reduce(es: List[Value]) = es match {
      case DoubleValue(i1)::DoubleValue(i2)::Nil => DoubleValue(Math.pow(i1, i2).toDouble)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $es")
    }
  }

  case object ToDouble extends BaseOp(List(Integer.TInteger), TDouble) {
    def reduce(es: List[Value]) = es match {
      case IntValue(i1)::Nil => DoubleValue(i1.toDouble)
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $es")
    }
  }

  case object Random extends BaseOp(List(), TDouble) {
    def reduce(es: List[Value]) = es match {
      case Nil => DoubleValue(Math.random())
      case _ => throw new SemanticException(s"wrong argument types for $getClass: $es")
    }
  }
  def NextRandom = BaseCall(Random)

  implicit def infixExpDoubleVar(e: Symbol) = InfixExp(Var(e))
  implicit def infixExpDoubleLit(e: Double) = InfixExp(BaseCall(DoubleLit(e)))
  implicit def infixExpDouble(e: Exp) = InfixExp(e)
  implicit def infixExpBaseCall(e: BaseCall) = InfixExp(e)
  case class InfixExp(e1: Exp) {
    def d = this
    def +(e2: Exp) = BaseCall(Plus, e1, e2)
    def +(e2: Int) = BaseCall(Plus, e1, e2)
    def +(e2: Symbol) = BaseCall(Plus, e1, e2)
    def -(e2: Exp) = BaseCall(Sub, e1, e2)
    def /(e2: Exp) = BaseCall(Div, e1, e2)
    def *(e2: Exp) = BaseCall(Mul, e1, e2)
    def pow(e2: Exp) = BaseCall(Pow, e1, e2)
    def toDouble = BaseCall(ToDouble, e1)
//    def pow(e2: InfixExp) = InfixExp(BaseCall(Pow, e1, e2.e1))
  }

}
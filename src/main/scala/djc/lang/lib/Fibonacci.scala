package djc.lang.lib

import util.Bag

import djc.lang.typ.Types._
import djc.lang.TypedSyntax._
import djc.lang.TypedSyntaxDerived._
import scala.collection.immutable.ListMap

import djc.lang.base.Bool._
import djc.lang.base.Integer._
import djc.lang.base.IntegerCompare._

object Fibonacci {
  def fibRec(n: Int): Int = 
    if (n <= 1)
      1
    else
      fibRec(n-1) + fibRec(n-2)

  def fibAcc(n: Int, acc: Int): Int =
    if (n <= 1)
      acc + 1
    else
      fibAcc(n-1, fibAcc(n-2, acc))

  def fibCont[R](n: Int, k: Int => R): R =
    if (n <= 1)
      k(1)
    else
      fibCont(n-1, v1 => fibCont(n-2, v2 => k(v1 + v2)))

  val fibType = TFun(TInteger, TInteger)
  val fib =
    ServerImpl(Rule(
        Bag(Pattern('fib, 'n -> TBase('Int), 'k -> TSvc(TBase('Int)))),
        Ifc(
          'n <== 1,
          'k!!(1),
          ('this~>'fib)!!(
            'n - 1,
            Def('K1, TSrv('k1 -> ?(TBase('Int))),
              ServerImpl(Rule(
                Bag('k1?('v1->TBase('Int))),
                ('this~>'fib)!!( // TODO 'this is wrong
                  'n - 2,
                  Def('K2, TSrv('k2 -> ?(TBase('Int))),
                    ServerImpl(Rule(
                      Bag('k2?('v2->TBase('Int))),
                      'k!!('v1+'v2))),
                    'K2~>'k2)))),
              'K1~>'k1)
          ))))~>'fib


}
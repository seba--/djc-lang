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
      fibCont(n-1,
        v1 =>
          fibCont(n-2,
            v2 =>
              k(v1 + v2)))

  val fibType = TFun(TInteger, TInteger)
  val fib =
    LocalService(
      'fib?('n -> TBase('Int), 'k -> ?(TBase('Int))),
      Let('FIB, fibType, 'this~>'fib) {
        Ifc('n <== 1) {'k!!(1)
        } Else { 'FIB!!(
            'n - 1,
            LocalService(
              'k1?('v1->TBase('Int)),
              'FIB!!(
                'n - 2,
                LocalService(
                  'k2?('v2->TBase('Int)),
                  'k!!('v1+'v2)))))
        }
      })

  val fibAlt =
    LocalService(
      'fib?('n -> TBase('Int), 'k -> ?(TBase('Int))),
      Let('FIB, fibType, 'this~>'fib) {
        Ifc('n <== 1) {
          'k !! (1)
        } Else {
          'FIB !!(
            'n - 1,
            LocalService(
              'k1 ? ('v1 -> TBase('Int)),
              'FIB !!(
                'n - 2,
                LocalService(
                  'k2 ? ('v2 -> TBase('Int)),
                  'k !! ('v1 + 'v2)))))
        }
      })
}
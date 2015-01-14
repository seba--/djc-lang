package djc.lang.lib

import util.Bag
import djc.lang.TypedLanguage._
import djc.lang.TypedLanguage.types._
import djc.lang.TypedSyntaxDerived._

import djc.lang.sem._
import djc.lang.base.Integer._
import djc.lang.AbstractTest


class TestFibonacci6 extends TestFibonacci(10, concurrent_6_thread.SemanticsFactory)


class TestFibonacci[V](max: Int, semFactory : ISemanticsFactory[V]) extends AbstractTest(semFactory) {

  testType("fib", Fibonacci.fib, Fibonacci.fibType)

  def testFib(n: Int) {
    val fibCall = Send(Fibonacci.fib, n, SpawnImg(PRINT_SERVER(TInteger))~>'PRINT)
    testType(s"fib_$n", fibCall, Unit)
    testInterp(s"fib_$n", fibCall, Set(Bag(PRINT(Fibonacci.fibAcc(n, 0)))))
  }

  for (i <- 0 to max)
    testFib(i)
}

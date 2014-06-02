package djc.lang.lib

import util.Bag
import djc.lang.TypedSyntax._
import djc.lang.TypedSyntaxDerived._

import djc.lang.sem._
import djc.lang.base.Integer._
import djc.lang.AbstractTest
import djc.lang.typ.Types._


class TestFibonacci1 extends TestFibonacci(10, nondeterm_1_subst.Semantics)
class TestFibonacci2 extends TestFibonacci(10, nondeterm_2_env.Semantics)
class TestFibonacci3 extends TestFibonacci(10, nondeterm_3_routed.SemanticsFactory)
class TestFibonacci4 extends TestFibonacci(10, nondeterm_4_grouped.SemanticsFactory)
class TestFibonacci5 extends TestFibonacci(10, nondeterm_5_parallel.SemanticsFactory)
class TestFibonacci6 extends TestFibonacci(10, concurrent_6_thread.SemanticsFactory)


class TestFibonacci[V](max: Int, semFactory : ISemanticsFactory[V]) extends AbstractTest(semFactory) {

  testType("fib", Fibonacci.fib, Fibonacci.fibType)

  def testFib(n: Int) {
    val fibCall = Send(Fibonacci.fib, n, PRINT_SERVER(TInteger) ~> 'PRINT)
    testType(s"fib_$n", fibCall, Unit)
    testInterp(s"fib_$n", fibCall, Set(Bag(PRINT(Fibonacci.fibAcc(n, 0)))))
  }

  for (i <- 0 to max)
    testFib(i)
}
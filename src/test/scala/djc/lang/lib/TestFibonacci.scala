package djc.lang.lib

import util.Bag
import djc.lang.TypedSyntax._
import djc.lang.TypedSyntaxDerived._

import djc.lang.sem.AbstractSemantics
import djc.lang.base.Integer._
import djc.lang.AbstractTest
import djc.lang.typ.Types._

import djc.lang.sem.nondeterm_1_subst
import djc.lang.sem.nondeterm_2_env
import djc.lang.sem.nondeterm_3_routed
import djc.lang.sem.nondeterm_4_grouped
import djc.lang.sem.nondeterm_5_parallel
import djc.lang.sem.concurrent_6_thread


class TestFibonacci1 extends TestFibonacci(3, nondeterm_1_subst.Semantics)
class TestFibonacci2 extends TestFibonacci(2, nondeterm_2_env.Semantics)
class TestFibonacci3 extends TestFibonacci(4, new nondeterm_3_routed.Semantics().newInstance())
class TestFibonacci4 extends TestFibonacci(4, new nondeterm_4_grouped.Semantics().newInstance())
//class TestFibonacci5 extends TestFibonacci(2, new nondeterm_5_parallel.Semantics().newInstance())
//class TestFibonacci6 extends TestFibonacci(2, new concurrent_6_thread.Semantics().newInstance(), false)


class TestFibonacci[V](max: Int, sem: AbstractSemantics[V], nondeterm: Boolean = true) extends AbstractTest(sem, nondeterm) {

  testType("fib", Fibonacci.fib, Fibonacci.fibType)

  def testFib(n: Int) {
    val fibCall = Send(Fibonacci.fib, n, PRINT_SERVER(TInteger) ~> 'PRINT)
    testType(s"fib_$n", fibCall, Unit)
    testInterp(s"fib_$n", fibCall, Set(Bag(PRINT(Fibonacci.fibAcc(n, 0)))))
  }

  for (i <- 0 to max) // larger than 3 -> out of memory for nondeterm_1_subst.Semantics
    testFib(i)


}
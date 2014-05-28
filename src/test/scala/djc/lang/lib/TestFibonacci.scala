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


class TestFibonacci1 extends TestFibonacci(nondeterm_1_subst.Semantics)
//class TestFibonacci2 extends TestFibonacci(nondeterm_2_env.Semantics)
//class TestFibonacci3 extends TestFibonacci(nondeterm_3_routed.Semantics)
//class TestFibonacci4 extends TestFibonacci(nondeterm_4_grouped.Semantics)
//class TestFibonacci5 extends TestFibonacci(nondeterm_5_parallel.Semantics)
//class TestFibonacci6 extends TestFibonacci(concurrent_6_thread.Semantics, false)


class TestFibonacci[V](sem: AbstractSemantics[V], nondeterm: Boolean = true) extends AbstractTest(sem, nondeterm) {

//  testType("fib", Fibonacci.fib, Fibonacci.fibType)


//  val fib0 = Send(Fibonacci.fib, 0, PRINT_SERVER ~> 'PRINT)
//
//  testInterp("fib0", fib0,
//    Set(Bag(Send(PRINT_SERVER ~> 'PRINT, 1)))
//  )


}
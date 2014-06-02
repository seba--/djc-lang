package djc.lang.lib.worker

import util.Bag
import djc.lang.TypedSyntax._
import djc.lang.TypedSyntaxDerived._

import djc.lang.sem._
import djc.lang.base.Integer._
import djc.lang.AbstractTest
import djc.lang.typ.Types._
import djc.lang.lib.Fibonacci


class TestBaseWorker1 extends TestBaseWorker(8, nondeterm_1_subst.Semantics)
class TestBaseWorker2 extends TestBaseWorker(8, nondeterm_2_env.Semantics)
class TestBaseWorker3 extends TestBaseWorker(8, nondeterm_3_routed.SemanticsFactory)
class TestBaseWorker4 extends TestBaseWorker(8, nondeterm_4_grouped.SemanticsFactory)
class TestBaseWorker5 extends TestBaseWorker(8, nondeterm_5_parallel.SemanticsFactory)
class TestBaseWorker6 extends TestBaseWorker(8, concurrent_6_thread.SemanticsFactory, false)


class TestBaseWorker[V](max: Int, semFactory : ISemanticsFactory[V], nondeterm: Boolean = true) extends AbstractTest(semFactory, nondeterm) {

  testType("fibWorker", BaseWorker.fibWorker, BaseWorker.fibWorkerType)

  def testFibWorker(n: Int) {
    val fibWokrerCall = Send(BaseWorker.fibWorker~>'work, n)
    testType(s"ffibWorker_$n", fibWokrerCall, Unit)
    testInterp(s"fibWorker_$n", fibWokrerCall, Set(Bag()))
  }

  for (i <- 0 to max)
    testFibWorker(i)


  testType("fibWorkerK", BaseWorker.fibWorkerK, BaseWorker.fibWorkerTypeK)

  def testFibWorkerK(n: Int) {
    val fibWokrerCall = Send(BaseWorker.fibWorkerK~>'work, n, PRINT_SERVER(TInteger)~>'PRINT)
    testType(s"fibWorkerK_$n", fibWokrerCall, Unit)
    testInterp(s"fibWorkerK_$n", fibWokrerCall, Set(Bag(PRINT(Fibonacci.fibAcc(n, 0)))))
  }

  for (i <- 0 to max)
    testFibWorkerK(i)

}
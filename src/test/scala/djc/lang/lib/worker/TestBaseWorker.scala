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

  testType("worker", Worker.worker, Worker.TWorker)
  testType("workerK", Worker.workerK, Worker.TWorkerK)

  testType("mkFibTask", Task.mkFibTask, Task.mkFibTaskType)
  def testmkFibTask(n: Int) {
    val mkFibTaskCall = Send(Task.mkFibTask, n, Worker.worker~>'work)
    testType(s"mkFibTask_$n", mkFibTaskCall, Unit)
    testInterp(s"mkFibTask_$n", mkFibTaskCall, Set(Bag()))
  }

  for (i <- 0 to max)
    testmkFibTask(i)


  testType("mkFibTaskK", Task.mkFibTaskK, Task.mkFibTaskTypeK)

  def testmkFibTaskK(n: Int) {
    val fibWokrerCall =
      Send(
        Task.mkFibTaskK,
        n,
        ServiceRef(
          LocalServerImpl(Rule(
            Bag(Pattern('cont, 'task -> Task.TTaskK(TInteger))),
            Worker.workerK(TInteger)~>'work!!('task, PRINT_SERVER(TInteger)~>'PRINT))),
          'cont))
    testType(s"mkFibTaskK_$n", fibWokrerCall, Unit)
    testInterp(s"mkFibTaskK_$n", fibWokrerCall, Set(Bag(PRINT(Fibonacci.fibAcc(n, 0)))))
  }

  for (i <- 0 to max)
    testmkFibTaskK(i)

}
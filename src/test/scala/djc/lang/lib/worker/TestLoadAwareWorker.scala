package djc.lang.lib.worker

import util.Bag
import djc.lang.Syntax
import djc.lang.TypedSyntax._
import djc.lang.TypedSyntaxDerived._

import djc.lang.sem._
import djc.lang.base.Integer._
import djc.lang.AbstractTest
import djc.lang.typ.Types._
import djc.lang.lib.Fibonacci


class TestLoadAwareWorker1 extends TestLoadAwareWorker(8, nondeterm_1_subst.Semantics)
class TestLoadAwareWorker2 extends TestLoadAwareWorker(8, nondeterm_2_env.Semantics)
class TestLoadAwareWorker3 extends TestLoadAwareWorker(8, nondeterm_3_routed.SemanticsFactory)
class TestLoadAwareWorker4 extends TestLoadAwareWorker(8, nondeterm_4_grouped.SemanticsFactory)
class TestLoadAwareWorker5 extends TestLoadAwareWorker(8, nondeterm_5_parallel.SemanticsFactory)
class TestLoadAwareWorker6 extends TestLoadAwareWorker(8, concurrent_6_thread.SemanticsFactory, false)


class TestLoadAwareWorker[V](max: Int, semFactory : ISemanticsFactory[V], nondeterm: Boolean = true) extends AbstractTest(semFactory, nondeterm) {

  testType("mkLoadAwareWorkerK", LoadAwareWorker.mkLoadAwareWorker, LoadAwareWorker.mkLoadAwareWorkerType)

  def testLoadAwareFibTaskK(n: Int) {
    val fibWorkerCall =
    LoadAwareWorker.mkLoadAwareWorker(TInteger)!!(Worker.workerK(TInteger),
      ServiceRef(
        LocalServerImpl(Rule(
          'withWorker?('worker -> (LoadAwareWorker.TLoadAwareWorkerK(TInteger) ++ TSrv('load -> ?(TInteger), 'done -> ?()))),
          Task.mkFibTaskK!!(n,
            ServiceRef(
              LocalServerImpl(Rule(
                'withTask?('task -> Task.TTaskK(TInteger)),
                'worker~>'work!!('task, PRINT_SERVER(TInteger)~>'PRINT))),
              'withTask)))),
        'withWorker))

    testType(s"mkFibTaskK_$n", fibWorkerCall, Unit)
    testInterp(s"mkFibTaskK_$n", fibWorkerCall, Set(Bag(PRINT(Fibonacci.fibAcc(n, 0)))), send => send.rcv.asInstanceOf[Syntax.ServiceRef].srv != PRINT_SERVER_NO)
  }

  for (i <- 0 to max)
    testLoadAwareFibTaskK(i)

}
package djc.lang.lib.worker

import util.Bag
import djc.lang.Syntax
import djc.lang.TypedLanguage._
import djc.lang.TypedLanguage.types._
import djc.lang.TypedSyntaxDerived._

import djc.lang.sem._
import djc.lang.base.Integer._
import djc.lang.AbstractTest
import djc.lang.lib.Fibonacci


class TestLoadAwareWorker6 extends TestLoadAwareWorker(8, concurrent_6_thread.SemanticsFactory)


class TestLoadAwareWorker[V](max: Int, semFactory : ISemanticsFactory[V]) extends AbstractTest(semFactory) {

  testType("mkLoadAwareWorkerK", LoadAwareWorker.mkLoadAwareWorker, LoadAwareWorker.mkLoadAwareWorkerType)

  def testLoadAwareFibTaskK(n: Int) {
    val fibWorkerCall =
    Let('Print, ?(TInteger), SpawnImg(PRINT_SERVER(TInteger))~>'PRINT)(
      LoadAwareWorker.mkLoadAwareWorker(TInteger)!!(Worker.workerK(TInteger),
        LocalService(
          'withWorker?('workerRep -> LoadAwareWorker.TLoadAwareWorkerK(TInteger)),
          Let('worker, TSrv(LoadAwareWorker.TLoadAwareWorkerK(TInteger)), SpawnImg('workerRep))(
            Task.mkFibTaskK!!(n,
              LocalService(
                'withTask?('task -> Task.TTaskK(TInteger)),
                'worker~>'init!!() && 'worker~>'work!!('task,
                  LocalService(
                    'whenDone?('res -> TInteger),
                    Par('Print!!('res), 'worker~>'getLoad!!('Print))))))))))

    testType(s"mkFibTaskK_$n", fibWorkerCall, Unit)
    testInterp(s"mkFibTaskK_$n",
      Par(fibWorkerCall),
      Set(Bag(PRINT(Fibonacci.fibAcc(n, 0)), PRINT(0)), Bag(PRINT(Fibonacci.fibAcc(n, 0)), PRINT(1))),
      send => send.rcv.asInstanceOf[Syntax.ServiceRef].srv != Syntax.SpawnImg(PRINT_SERVER_NO))
  }

  for (i <- 0 to max)
    testLoadAwareFibTaskK(i)

}

package djc.lang.lib.worker

import djc.lang.typ.Types._
import djc.lang.TypedSyntax._
import djc.lang.TypedSyntaxDerived._
import util.Bag
import djc.lang.lib.worker.Task._
import djc.lang.lib.worker.Worker._
import djc.lang.base.Integer._

object LoadAwareWorker {

  val TLoadAwareWorker = TSrvRep('work -> ?(TTask), 'getLoad -> ?(?(TInteger)), 'init -> ?())
  val TLoadAwareWorkerK = TUniv('K, TSrvRep('work -> ?(TTaskK('K), ?('K)), 'getLoad -> ?(?(TInteger)), 'init -> ?()))

  val mkLoadAwareWorkerType = TUniv('K, TFun(TWorkerK('K), TLoadAwareWorkerK('K)))
  val mkLoadAwareWorker = TAbs('K, LocalService(
    'make?('workerRep -> TWorkerK('K), 'k -> ?(TLoadAwareWorkerK('K))),
    Def('worker, TSrv(TWorkerK('K)), SpawnLocal('workerRep),
      Def('laWorker, TLoadAwareWorkerK('K) ++ TSrvRep('load -> ?(TInteger)), // internally we know that there is a 'load service
        ServerImpl(
          Rule(
            'init?(),
            'this~>'load!!(0)
          ),
          Rule(
            'work?('task -> TTaskK('K), 'k -> ?('K)) && 'load?('n -> TInteger),
            'this~>'load!!('n + 1) &&
             Def('notifyDone, ?(), 'this~>'done,
               'worker~>'work!!('task,
                 LocalService('cont?('res -> 'K),'notifyDone!!() && 'k!!('res))))
          ),
          Rule(
            'done?() && 'load?('n -> TInteger),
            'this~>'load!!('n - 1)
          ),
          Rule(
            'getLoad?('notifyLoad -> ?(TInteger)) && 'load?('n -> TInteger),
            'notifyLoad!!('n) && 'this~>'load!!('n)
          )
        ),
        'k!!('laWorker cast (TLoadAwareWorkerK('K)))
      )
    )
  )
  )
}
package djc.lang.lib.worker

import djc.lang.typ.Types._
import djc.lang.TypedSyntax._
import djc.lang.TypedSyntaxDerived._
import util.Bag
import djc.lang.lib.worker.Task._
import djc.lang.lib.worker.Worker._
import djc.lang.base.Integer._

object LoadAwareWorker {

  val TLoadAwareWorker = TSrv('work -> ?(TTask), 'getLoad -> ?(?(TInteger)))
  val TLoadAwareWorkerK = TUniv('K, TSrv('work -> ?(TTaskK('K), ?('K)), 'getLoad -> ?(?(TInteger))))

  val mkLoadAwareWorkerType = TUniv('K, TFun(TWorkerK('K), TLoadAwareWorkerK('K) ++ TSrv('load -> ?(TInteger), 'done -> ?())))
  val mkLoadAwareWorker = TAbs('K, LocalServerImpl(
    Rule(
      'make?('worker -> TWorkerK('K), 'k -> ?(TLoadAwareWorkerK('K) ++ TSrv('load -> ?(TInteger), 'done -> ?()))),
      Def('laWorker, TLoadAwareWorkerK('K) ++ TSrv('load -> ?(TInteger), 'done -> ?()),
        LocalServerImpl(
          Rule(
            'work?('task -> TTaskK('K), 'k -> ?('K)) && 'load?('n -> TInteger),
            'this~>'load!!('n + 1) &&
             Def('outer, TLoadAwareWorkerK('K) ++ TSrv('load -> ?(TInteger), 'done -> ?()), 'this,
               'worker~>'work!!('task, ServiceRef(
               LocalServerImpl(Rule(
                 'cont?('res -> 'K),
                 ('outer~>'done!!()) && ('k!!('res))
               )),
              'cont)))
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
        Par('laWorker~>'load!!(0), 'k!!('laWorker))
      )
    )
  )~>'make)
}
package djc.lang.lib.worker

import djc.lang.typ.Types._
import djc.lang.TypedSyntax._
import djc.lang.TypedSyntaxDerived._
import djc.lang.lib.worker.Task._

object Worker {

  val TWorker = TSrv('work -> ?(TTask))
  val TWorkerK = TUniv('K, TSrv('work -> ?(TTaskK('K), ?('K))))
  val TWorkerN = TUniv('K, TSrv('work -> ?(TTaskK('K), ?())))

  val worker = ServerImpl(
    Rule(
      'work?('task -> TTask),
      'task!!()
    )
  )

  val workerK = TAbs('K, ServerImpl(
    Rule(
      'work?('task -> TTaskK('K), 'k -> ?('K)),
      'task!!('k)
    )
  ))

  val workerN = TAbs('K, ServerImpl(
    Rule(
      'work?('task -> TTaskK('K), 'k -> ?()),
      'task!!(LocalService('notifyDone?('k2 -> ?('K)), 'k!!()))
    )
  ))

}
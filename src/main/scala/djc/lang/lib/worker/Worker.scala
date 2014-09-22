package djc.lang.lib.worker

import djc.lang.TypedLanguage._
import djc.lang.TypedSyntaxDerived._
import djc.lang.lib.worker.Task._

object Worker {

  val TWorker = TSrvRep('work -> ?(TTask), 'init -> ?())
  val TWorkerK = TUniv('K, TSrvRep('work -> ?(TTaskK('K), ?('K)), 'init -> ?()))

  val worker = ServerImpl(
    Rule('init?(), Par()),
    Rule(
      'work?('task -> TTask),
      SpawnLocal('task)~>'run!!()
    )
  )

  val workerK = TAbs('K, ServerImpl(
    Rule('init?(), Par()),
    Rule(
      'work?('task -> TTaskK('K), 'k -> ?('K)),
      SpawnLocal('task)~>'run!!('k)
    )
  ))

}
package djc.lang.lib.worker

import djc.lang.typ.Types._
import djc.lang.TypedSyntax._
import djc.lang.TypedSyntaxDerived._
import djc.lang.lib.worker.Task._

object Worker {

  val TWorker = TSrvRep('work -> ?(TTask))
  val TWorkerK = TUniv('K, TSrvRep('work -> ?(TTaskK('K), ?('K))))

  val worker = ServerImpl(
    Rule(
      'work?('task -> TTask),
      SpawnLocal('task)~>'run!!()
    )
  )

  val workerK = TAbs('K, ServerImpl(
    Rule(
      'work?('task -> TTaskK('K), 'k -> ?('K)),
      SpawnLocal('task)~>'run!!('k)
    )
  ))

}
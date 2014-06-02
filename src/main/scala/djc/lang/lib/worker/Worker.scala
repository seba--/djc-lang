package djc.lang.lib.worker

import djc.lang.typ.Types._
import djc.lang.TypedSyntax._
import util.Bag
import djc.lang.lib.worker.Task._

object Worker {

  val TWorker = TSrv('work -> ?(TTask))
  val TWorkerK = TUniv('K, TSrv('work -> ?(TTaskK('K), ?('K))))

  val worker = ServerImpl(
    Rule(
      Bag(Pattern('work, 'task -> TTask)),
      'task!!()
    )
  )

  val workerK = TAbs('K, ServerImpl(
    Rule(
      Bag(Pattern('work, 'task -> TTaskK('K), 'k -> ?('K))),
      'task!!('k)
    )
  ))


}
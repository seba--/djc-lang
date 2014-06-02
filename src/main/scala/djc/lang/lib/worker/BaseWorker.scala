package djc.lang.lib.worker

import djc.lang.typ.Types._
import djc.lang.TypedSyntax._
import djc.lang.TypedSyntaxDerived._
import djc.lang.base.Integer._
import util.Bag
import djc.lang.lib.Fibonacci
import djc.lang.lib.Function

object BaseWorker {

  val TWorker = TUniv('V, TSrv('work -> ?('V)))
  val TWorkerK = TUniv('V, TUniv('K, TSrv('work -> TFun('V, 'K))))

  val fibWorkerType = TWorker(TInteger)
  val fibWorker = ServerImpl(
    Rule(
      Bag(Pattern('work, 'n -> TInteger)),
      Fibonacci.fib!!('n, Function.consume(TInteger)))
  )


  val fibWorkerTypeK = TWorkerK(TInteger, TInteger)
  val fibWorkerK = ServerImpl(
    Rule(
      Bag(Pattern('work, 'n -> TInteger, 'k -> ?(TInteger))),
      Fibonacci.fib!!('n, 'k))
  )

}
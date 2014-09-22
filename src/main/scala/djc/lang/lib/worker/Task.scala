package djc.lang.lib.worker

import djc.lang.TypedLanguage._
import djc.lang.TypedLanguage.types._
import djc.lang.TypedSyntaxDerived._
import djc.lang.base.Integer._
import djc.lang.lib.Fibonacci
import djc.lang.lib.Function

object Task {

  val TTask = TSrvRep('run -> ?())
  val TTaskK = TUniv('K, TSrvRep('run -> ?(?('K))))

  val mkFibTaskType = TFun(TInteger, TTask)
  val mkFibTask = LocalService(
      'make?('n -> TInteger, 'k -> ?(TTask)),
      'k!!(ServerImpl(Rule('run?(), Fibonacci.fib!!('n, Function.consume(TInteger))))))


  val mkFibTaskTypeK = TFun(TInteger, TTaskK(TInteger))
  val mkFibTaskK = LocalService(
      'make?('n -> TInteger, 'k -> ?(TTaskK(TInteger))),
      'k!!(ServerImpl(Rule('run?('k2 -> ?(TInteger)), Fibonacci.fib!!('n, 'k2)))))
}
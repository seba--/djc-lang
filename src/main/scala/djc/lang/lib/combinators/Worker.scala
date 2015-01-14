package djc.lang.lib.combinators

import djc.lang.TypedLanguage._
import djc.lang.TypedLanguage.types._
import djc.lang.TypedSyntaxDerived.{TThunk => _, _}

object Worker {
  val workerK = TAbs('K, ServerImpl(
    Rule('init?(), Par()),
    Rule(
      'work?('thunk -> TThunk('K), 'k -> ?('K)),
      SpawnLocalImg('thunk)~>'$force!!('k)
    )
  ))
}

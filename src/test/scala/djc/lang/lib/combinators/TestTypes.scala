package djc.lang.lib.combinators

import djc.lang.TypeTests
import djc.lang.lib.combinators.halt.MkStoppable
import djc.lang.lib.combinators.loadbalance.{MkRoundRobin, MkLoadAware, MkBalanced}
import djc.lang.lib.combinators.migration.MkHost
import djc.lang.lib.combinators.recovery.MkRecover
import djc.lang.lib.combinators.aux.{MapK, ForEach, FilterK, AnyK}

class TestTypes extends TypeTests {


  val tests: List[Combinator] = List(
    AnyK, FilterK, ForEach, MapK, MkStoppable, MkBalanced, MkLoadAware, MkRoundRobin, MkHost, MkRecover
  )

  tests.foreach { combinator =>
    testType(combinator.name, combinator.impl, combinator.tpe)
  }


}

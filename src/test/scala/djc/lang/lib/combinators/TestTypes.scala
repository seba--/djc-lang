package djc.lang.lib.combinators

import djc.lang.TypeTests
import djc.lang.lib.combinators.halt.MkStoppable
import djc.lang.lib.combinators.loadbalance.{MkBalanced, MkLoadAware, MkRoundRobin}
import djc.lang.lib.combinators.migration.MkHost
import djc.lang.lib.combinators.recovery.MkRecover
import djc.lang.lib.combinators.aux._

class TestTypes extends TypeTests {


  val tests: List[Combinator] = List(
    AnyK, FilterK, ForEach, MapK, MkStoppable, MkBalanced, MkLoadAware, MkRoundRobin, MkHost, MkRecover, FoldK
  )

  tests.foreach { combinator =>
    testType(combinator.name, combinator.impl, combinator.tpe)
  }


}

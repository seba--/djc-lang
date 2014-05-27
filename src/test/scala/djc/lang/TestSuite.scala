package djc.lang

import org.scalatest.FunSuite

import djc.lang.sem._


trait TestSpec {
  def run[V](sem: AbstractSemantics[V], nondeterm: Boolean = true)
}

abstract case class TestSuite[V](spec: TestSpec) extends FunSuite {
  spec.run(nondeterm_1_subst.Semantics)
  spec.run(nondeterm_2_env.Semantics)
  spec.run(nondeterm_3_routed.Semantics)
  spec.run(nondeterm_4_grouped.Semantics)
  spec.run(nondeterm_5_parallel.Semantics)
  spec.run(concurrent_6_thread.Semantics, false)
}
package djc.lang

import org.scalatest.FunSuite

import djc.lang.sem._


trait TestSpec {
  def run[V](sem: AbstractSemantics[V], nondeterm: Boolean = true)
}

abstract case class TestSuite[V](spec: TestSpec) extends FunSuite {
}
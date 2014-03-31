package djc.lang

import scala.language.higherKinds

import util.Bag

trait AbstractSemantics[V] {
  type Val = V
  def normalizeVal(v: Val): Bag[Send]

  type Res[T] = Set[T] // nondeterminstic result as set of values

  def interp(p: Prog): Res[Val]
}
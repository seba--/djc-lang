package djc.lang.sem

import scala.language.higherKinds

import util.Bag
import djc.lang.{Prog, Send}

trait AbstractSemantics[V] {
  type Val = V
  def normalizeVal(v: Val): Bag[Send]

  type Res[T] = Set[T] // nondeterminstic result as set of values

  def interp(p: Prog): Res[Val]
}
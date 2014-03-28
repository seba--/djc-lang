package djc.lang

import scala.language.higherKinds

import util.Bag

trait AbstractSemantics[V] {
  type Val = V
  def normalizeVal(v: Val): Bag[Send]

  type Res[T] = Set[T] // nondeterminstic result as set of values

  def interp(p: Prog): Res[Val]

  def nondeterministic[T,U](ts: Res[T], f: T => Res[U]): Res[U] =
    (ts map f).flatten
}
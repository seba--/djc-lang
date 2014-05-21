package djc.lang.sem

import util.Bag
import djc.lang.FlatSyntax._

trait AbstractSemantics[V] {
  type Val = V
  def normalizeVal(v: Val): Bag[Send]

  type Res[T] = Set[T] // nondeterminstic result as set of values

  def interp(p: Prog): Res[Val]


}

case class SemanticException(msg: String) extends RuntimeException(msg)
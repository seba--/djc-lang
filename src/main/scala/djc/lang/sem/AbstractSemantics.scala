package djc.lang.sem

import util.Bag
import djc.lang.Syntax._

trait AbstractSemantics[V] {
  type Val = V
  type Res[T] = Set[T] // nondeterminstic result as set of values

  def interp(p: Exp): Res[Val]

  def normalizeVal(v: Val): Bag[Send]
}

case class SemanticException(msg: String) extends RuntimeException(msg)
package djc.lang.sem

import util.Bag
import djc.lang.Syntax._

object AbstractSemantics {
  type Res[T] = Set[T] // nondeterminstic result as set of values
}

trait AbstractSemantics[V] {
  type Val = V
  type Res[T] = AbstractSemantics.Res[T]

  def interp(p: Exp): Res[Val]

  def normalizeVal(v: Val): Bag[Send]
}

case class SemanticException(msg: String) extends RuntimeException(msg)
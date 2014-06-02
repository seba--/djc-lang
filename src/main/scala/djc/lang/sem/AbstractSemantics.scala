package djc.lang.sem

import util.Bag
import djc.lang.Syntax._
import scala.language.higherKinds

object AbstractSemantics {
  type Res[T] = Set[T] // nondeterminstic result as set of values
}

trait AbstractSemantics[V] {
  type Val = V

  type Res[_]// = AbstractSemantics.Res[T]
  def resToSet[T](res: Res[T]): Set[T]

  def interp(p: Par): Res[Val]

  def normalizeVal(v: Val): Bag[Send]

  val isFullyNondeterministic: Boolean
}

case class SemanticException(msg: String) extends RuntimeException(msg)
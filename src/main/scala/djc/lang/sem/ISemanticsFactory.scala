package djc.lang.sem

import util.Bag
import djc.lang.Syntax._

trait ISemanticsFactory[V] {
  def newInstance(): AbstractSemantics[V]
}

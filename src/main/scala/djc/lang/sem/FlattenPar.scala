package djc.lang

import djc.lang.Syntax._
import util.Bag

object FlattenPar extends Mapper {

  override def map(e: Exp) = e match {
    case Par(es) => Par(flattenPars(es))
    case e => super.map(e)
  }

  def flattenPars(es: Bag[Exp]) =
    es flatMap (map(_) match {case Par(es) => es; case e => Bag(e)})

}
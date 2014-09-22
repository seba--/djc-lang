package djc.lang.lib.combinators

import djc.lang.TypedLanguage.Exp
import djc.lang.TypedLanguage.types._


trait Combinator {
  def name = this.getClass.getName
  val impl: Exp
  val tpe: Type
}

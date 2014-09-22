package djc.lang.lib.combinators

import djc.lang.TypedLanguage.{Exp, Type}


trait Combinator {
  def name = this.getClass.getName
  val impl: Exp
  val tpe: Type
}

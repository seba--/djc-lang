package djc.lang.lib.combinators

import djc.lang.TypedSyntax.Exp
import djc.lang.typ.Types.Type


trait Combinator {
  def name = this.getClass.getName
  val impl: Exp
  val tpe: Type
}

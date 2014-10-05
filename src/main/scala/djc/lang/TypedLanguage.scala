package djc.lang

import djc.lang.typ._

object TypedLanguage extends TypedSyntaxFamily {
  val types = Types
  val op = new DefaultTypedSyntaxOps {
    val syntax = TypedLanguage
  }
}

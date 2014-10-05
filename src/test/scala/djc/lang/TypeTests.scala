package djc.lang

import djc.lang.TypedLanguage.Exp
import djc.lang.TypedLanguage.types.Type
import djc.lang.typ.Checker._
import org.scalatest.FunSuite

trait TypeTests extends FunSuite {
  def testType(s: String, p: Exp, expected: Type, gamma: Context=Map(), tgamma: TVarContext=Map()) = {
    test(s ++ "-type") {
      try {
        val tpe = typeCheck(gamma, tgamma, p)
        assert(subtype(tgamma)(tpe,expected), s"subtype is wrong: $tpe </< $expected")
      } catch {
        case TypeCheckException(msg) => assert(false, msg)
      }
    }
  }
}

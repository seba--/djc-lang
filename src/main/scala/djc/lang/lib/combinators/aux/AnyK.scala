package djc.lang.lib.combinators.aux

import djc.lang.TypedLanguage._
import djc.lang.TypedSyntaxDerived._
import djc.lang.base.Bool._
import djc.lang.base.Lists._
import djc.lang.lib.combinators.Combinator

object AnyK extends Combinator {
  def apply(t: Type) = TApp(impl, t)

  val TAnyK = TUniv('A, TFun(TList('A), TFun(TVar('A) -> TBool), TBool))
  val TAnyKInt = TUniv('A, TSrv(TSrvRep('ak -> TFun(TList('A), TBool))))

  val tpe = TAnyK
  val impl =
    TAbs('A) {
      LocalServer {
        Rule('anyk?('xs -> TList('A), 'fun -> TFun(TVar('A) -> TBool), 'k -> ?(TBool))) {
          LocalServer {
            Rule('ak?('xs -> TList('A), 'k -> ?(TBool))) {
              Ifc(TAnyKInt('A))('xs.isNil('A)) {
                'k!!fal()
              } Else {
                Letk('check, TBool, 'fun !! ('xs.head('A))) {
                    Ifc('check) {
                      'k!!tru()
                    } Else {
                      'this~>'ak!!('xs.tail('A), 'k)
                    }
                }
              }
            }
          }~>'ak!!('xs, 'k)
        }
      }~>'anyk
    }

}

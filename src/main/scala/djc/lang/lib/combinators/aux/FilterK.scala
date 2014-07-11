package djc.lang.lib.combinators.aux

import djc.lang.TypedSyntax._
import djc.lang.TypedSyntaxDerived._
import djc.lang.base.Lists._
import djc.lang.base.Bool._
import djc.lang.typ.Types._

object FilterK {
  def apply(t: Type) = TApp(filterk, t)

  val TFilterK = TUniv('A, TFun(TList('A), TFun(TVar('A) -> TBool), TList('A)))
  val TFilterKInt = TUniv('A, TSrv(TSrvRep('fk -> TFun(TList('A), TList('A)))))

  val filterk =
    TAbs('A) {
      LocalServer {
        Rule('filterk?('xs -> TList('A), 'fun -> TFun(TVar('A) -> TBool), 'k -> ?(TList('A)))) {
          LocalServer {
            Rule('fk?('xs -> TList('A), 'k -> ?(TList('A)))) {
              Ifc(TFilterKInt('A))('xs.isNil('A)) {
                'k!!nil('A)
              } Else {
                Letk('keep, TBool, 'fun !! ('xs.head('A))) {
                  Letk('ys, TList('A), 'this~>'fk !! ('xs.tail('A))) {
                    Ifc('keep) {
                      'k!!(('xs.head('A), TVar('A)) :: 'ys)
                    } Else {
                      'k!!'ys
                    }

                  }
                }
              }
            }
          }~>'fk!!('xs, 'k)
        }
      }~>'filterk
    }

}


package djc.lang.lib.combinators.aux

import djc.lang.TypedSyntax._
import djc.lang.TypedSyntaxDerived._
import djc.lang.base.Lists._
import djc.lang.typ.Types._

object MapK {
  def apply(t: Type, t2: Type) = TApp(mapk, t, t2)

  val TMapK = TUniv('A, TUniv('B, TFun(TList('A), TFun(TVar('A) -> TVar('B)), TList('B))))
  val TMapKInt = TUniv('A, TUniv('B, TSrv(TSrvRep('mapkint -> TFun(TList('A), TList('B))))))

  val mapk =
  TAbs('A, 'B) {
    LocalServer {
      Rule('mapk?('xs -> TList('A), 'fun -> TFun(TVar('A) -> TVar('B)), 'k -> ?(TList('B)))) {
        LocalServer {
          Rule('mapkint?('xs -> TList('A), 'k -> ?(TList('B)))) {
            Ifc(TMapKInt('A, 'B))('xs.isNil('A)) {
              'k!!nil('B)
            } Else {
              Letk('y, 'B, 'fun !! ('xs.head('A))) {
                Letk('ys, TList('B), 'this~>'mapkint !! ('xs.tail('A))) {
                  'k!!((Var('y), TVar('B)) :: 'ys)
                }
              }
            }
          }
        }~>'mapkint!!('xs, 'k)
      }
    }~>'map
  }

}

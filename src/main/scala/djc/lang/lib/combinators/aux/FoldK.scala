package djc.lang.lib.combinators.aux

import djc.lang.TypedLanguage._
import djc.lang.TypedLanguage.types._
import djc.lang.TypedSyntaxDerived._
import djc.lang.base.Lists._
import djc.lang.lib.combinators.Combinator


/**
  * Created by oliver on 04.08.16.
  */
object FoldK extends Combinator {
  def apply(t: Type, t2: Type) = TApp(impl, t, t2)

  val TFoldK = TUniv('A, TUniv('B, TFun(TList('A), TVar('B), TFun(TVar('B), TVar('A), TVar('B)), 'B)))
  val TFoldKInt = TUniv('A, TUniv('B, TSrv(TSrvRep('foldkint -> TFun(TList('A), TVar('B), TVar('B))))))

  val tpe = TFoldK
  val impl =
    TAbs('A, 'B) {
      LocalServer {
        Rule('foldk?('xs -> TList('A), 'init -> TVar('B), 'fun -> TFun(TVar('B), TVar('A), TVar('B)), 'k -> ?('B))) {
          LocalServer {
            Rule('foldkint?('xs -> TList('A), 'init -> TVar('B), 'k -> ?('B))) {
              Ifc(TFoldKInt('A, 'B))('xs.isNil('A)) {
                'k !! 'init
              } Else {
                Letk('y, 'B, 'fun !! ('init, 'xs.head('A))) {
                  'this~>'foldkint !! ('xs.tail('A), 'y, 'k)
                }
              }
            }
          }~>'foldkint!!('xs, 'init, 'k)
        }
      }~>'foldk
    }

}
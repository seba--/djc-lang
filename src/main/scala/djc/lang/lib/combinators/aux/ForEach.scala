package djc.lang.lib.combinators.aux


import djc.lang.TypedLanguage._
import djc.lang.TypedLanguage.types._
import djc.lang.TypedSyntaxDerived._
import djc.lang.base.Lists._
import djc.lang.lib.combinators.Combinator
import djc.lang.lib.Function

object ForEach extends Combinator {
  def apply(t: Type) = TApp(impl, t)

  val TForEachInt = TUniv('A, TSrv(TSrvRep('foreach -> ?(TList('A)))))
  val TForEach = TUniv('A, ?(TList('A), TFun(TVar('A) -> Unit)))


  val tpe = TForEach
  val impl = TAbs('A) {
    LocalServer {
      Rule('foreach?('xs -> TList('A), 'body -> TFun(TVar('A) -> Unit))) {
        LocalServer {
          Rule('foreach?('xs -> TList('A))) {
            Ifc(TForEachInt('A))('xs.isNil('A)) {
              Par()
            } Else {
              'body !!('xs.head('A), Function.consume(Unit)) && 'this ~> 'foreach !! ('xs.tail('A))
            }
          }
        }~>'foreach!!('xs)
      }
    }~>'foreach
  }

}

package djc.lang.lib.combinators.aux


import djc.lang.TypedSyntax._
import djc.lang.TypedSyntaxDerived._
import djc.lang.base.Lists._
import djc.lang.typ.Types._
import djc.lang.lib.Function

object ForEach {
  def apply(t: Type) = TApp(service, t)

  val TForEachInt = TUniv('A, TSrv(TSrvRep('foreach -> ?(TList('A), TFun(TVar('A) -> Unit)))))
  val TForEach = TUniv('A, ?(TList('A), TFun(TVar('A) -> Unit)))

  val service = TAbs('A) {
    LocalServer {
      Rule('foreach?('xs -> TList('A), 'body -> TFun(TVar('A) -> Unit))) {
        Ifc(TForEachInt('A))('xs.isNil('A)) {
          Par()
        } Else {
          'body!!('xs.head('A), Function.consume(Unit)) && 'this~>'foreach!!('xs.tail('A))
        }
      }
    }~>'foreach
  }

}

package djc.lang.lib.combinators.loadbalance

import djc.lang.TypedLanguage._
import djc.lang.TypedSyntaxDerived.{TThunk => _, _}
import djc.lang.base.Integer._
import djc.lang.base.Lists._
import djc.lang.lib.combinators.Combinator

object MkRoundRobin extends Combinator {
  def apply(t1: Type) = TApp(impl, t1)

  val TInstL = TUniv('W, TList(TSrv('W)))
  val TInternal = TUniv('A, TSrv(TSrvRep('next -> TFun(TList('A) -> 'A), 's -> ?(TInteger))))

  val tpe = TUniv('A, TSrvRep('make -> ?(?(TFun(TList('A) -> 'A)))))
  val impl = TAbs('A) {
    ServerImpl {
      Rule('make?('k -> ?(TFun(TList('A) -> 'A)))) {
        Let('rr, TInternal('A),
          LocalServer (
            Rule('next?('ws -> TList('A), 'k -> ?('A)), 's?('i -> TInteger)) {
              'k!!('ws.elemAt('A)('i)) && 'this~>'s!!(('i + 1) mod 'w.length('A))
            })) {
          'rr~>'s!!(0) && 'k!!('rr~>'next)
        }
      }
    }
  }
}
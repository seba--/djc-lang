package djc.lang.lib.combinators.loadbalance

import djc.lang.TypedSyntax._
import djc.lang.TypedSyntaxDerived.{TThunk => _, _}
import djc.lang.base.Integer._
import djc.lang.base.Lists._
import djc.lang.typ.Types._

object MkRoundRobin {
  def apply(t1: Type) = TApp(combinator, t1)

  val TInstL = TUniv('W, TList(TSrv('W)))
  val TInternal = TUniv('A, TSrvRep('next -> TFun(TList('A) -> 'A), 's -> ?(TInteger)))

  val combinator = TAbs('A) {
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
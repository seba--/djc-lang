package djc.lang.lib.combinators.halt

import djc.lang.TypedSyntax._
import djc.lang.TypedSyntaxDerived.{TThunk => _, _}
import djc.lang.lib.combinators._
import djc.lang.typ.Types._

object MkStoppable {
  def apply(t1: Type, t2: Type) = TApp(combinator, t1, t2)

  val TInternal = TUniv('A, TUniv('W, TWorker('A),
                   TSrvRep('init -> ?(), 'work -> TFun(TThunk('A) -> 'A),
                           'stop -> ?(), 'alive -> ?(), 'instance -> ?(TSrv('W)))))

  val combinator = TAbs('A, 'W << TWorker('A)) {
    ServerImpl {
      Rule('make?('worker -> 'W, 'k -> ?(TStWorker('A)))) {
        Let('stoppable, TStWorker('A),
          (ServerImpl (
            Rule('init?()) {
              Let(TInternal('A, 'W))('w, TSrv('W), SpawnLocal('worker)) {
                'w~>'init!!() && 'this~>'instance!!'w && 'this~>'alive!!()
              }
            },
            Rule('work?('thunk -> TThunk('A), 'k -> ?('A)),
                 'instance?('w -> TSrv('W)),
                  'alive?()) {
              'w~>'work!!'thunk && 'this~>'instance!!'w && 'this~>'alive!!()
            },
            Rule('stop?(), 'alive?()) {
              Par()
            }))) {
          'k!!'stoppable
        }
      }
    }
  }
}

package djc.lang.lib.combinators.halt

import djc.lang.TypedLanguage._
import djc.lang.TypedLanguage.types._
import djc.lang.TypedSyntaxDerived.{TThunk => _, _}
import djc.lang.lib.combinators._

object MkStoppable extends Combinator {
  def apply(t1: Type, t2: Type) = TApp(impl, t1, t2)

  val TMkStoppable = TUniv('A, TUniv('W, TWorker('A), TSrvRep('make -> ?('W, ?(TStWorker('A))))))
  val TInternal = TUniv('A, TUniv('W, TWorker('A),
                   TSrv(TSrvRep('init -> ?(), 'work -> TFun(TThunk('A) -> 'A),
                           'stop -> ?(), 'alive -> ?(), 'instance -> ?(TSrv('W)))))  )

  val tpe = TMkStoppable
  val impl = TAbs('A, 'W << TWorker('A)) {
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
              'w~>'work!!('thunk,'k) && 'this~>'instance!!'w && 'this~>'alive!!()
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

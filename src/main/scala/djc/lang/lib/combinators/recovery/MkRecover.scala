package djc.lang.lib.combinators.recovery

import djc.lang.TypedLanguage._
import djc.lang.TypedLanguage.types._
import djc.lang.TypedSyntaxDerived.{TThunk => _, _}
import djc.lang.base.Lists._
import djc.lang.base.Pairs._
import djc.lang.base.Integer._
import djc.lang.base.IntegerCompare._
import djc.lang.base.Bool._
import djc.lang.lib.combinators._
import djc.lang.lib.combinators.aux._

object MkRecover extends Combinator {
  def apply(t1: Type, t2: Type) = TApp(impl, t1, t2)

  val TP = TTuple(TInteger, TInteger, TThunk('A), ?('A))
  val TInternal = TUniv('A, TUniv('W, TWorker('A),
    TSrv(TSrvRep('init -> ?(), 'work -> TFun(TThunk('A) -> 'A), 'inst -> ?(TSrv('W)),  'pending -> ?(TList(TP)), 'done -> ?(TInteger)))))


  val tpe = TUniv('A, TUniv('W, TStWorker('A), TSrvRep('make -> ?('W, TInteger, ?(TWorker('A))))))
  val impl = TAbs('A, 'W << TStWorker('A)) {
    ServerImpl {
      Rule('make?('worker -> 'W, 'timeout -> TInteger, 'k -> ?(TWorker('A)))) {
        Let('selfrecovering, TWorker('A),
          ServerImpl (
            Rule('init?()) {
              Let(TInternal('A, 'W))('w, TSrv('W), SpawnLocalImg('worker)) {
                'w~>'init!!() && 'this~>'inst!!'w && 'this~>'pending!!nil(TP)
              }
            },
            Rule('work?('thunk -> TThunk('A), 'k -> ?('A)),
                 'inst?('w -> TSrv('W)), 'pending?('xs -> TList(TP))) {
               Let(TInternal('A,'W))('id, TInteger, freshInt()) {
                 ('this~>'pending!!((pair(TP,'id, localTime(), 'thunk, 'k), TP) :: 'xs.l)
                   && 'this~>'inst!!'w
                   && Letk('r, 'A, 'w~>'work!!'thunk) {
                        'k!!'r && 'this~>'done!!'id
                      }
                   )
               }
            },
            Rule('done?('id -> TInteger), 'pending?('xs -> TList(TP))) {
              FilterK(TP)!!('xs, Lambda('p, TP -> TBool, fst(TP, 'p) <> 'id), 'this~>'pending)
            },
            Rule('pending?('xs -> TList(TP)),'inst?('w -> TSrv('W))) {
              Let(TInternal('A,'W))('time, TInteger, localTime()) {
                Let('work, TFun(TThunk('A) -> 'A), 'this~>'work) {
                  Letk('late, TBool, AnyK(TP) !!('xs, Lambda('p, TP -> TBool, ('time - snd(TP, 'p)) > 'timeout))) {
                    Ifc('late) {
                      'w ~> 'stop !!() && 'this ~> 'init !!() && ForEach(TP) !!('xs, Lambda('p, TP -> Unit, 'work!!(thrd(TP, 'p), frth(TP, 'p))))
                    } Else {
                      'this~>'pending!!'xs && 'this~>'inst!!'w
                    }
                  }
                }
              }
            })) {
          'k!!'selfrecovering
        }
      }
    }
  }
}

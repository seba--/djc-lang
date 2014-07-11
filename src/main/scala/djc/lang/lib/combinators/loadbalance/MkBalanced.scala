package djc.lang.lib.combinators.loadbalance

import djc.lang.TypedSyntax._
import djc.lang.TypedSyntaxDerived.{TThunk => _, _}
import djc.lang.base.Lists._
import djc.lang.lib.combinators._
import djc.lang.lib.combinators.aux.{ForEach, MapK}
import djc.lang.typ.Types._

object MkBalanced {
  def apply(t1: Type, t2: Type) = TApp(combinator, t1, t2)

  val TInstL = TUniv('W, TList(TSrv('W)))
  val TInternal = TUniv('A, TUniv('W, TWorker('A),
    TSrvRep('init -> ?(), 'work -> TFun(TThunk('A) -> 'A), 'insts -> ?(TInstL('W)))))


  val combinator = TAbs('A, 'W << TLaWorker('A)) {
    ServerImpl {
      Rule('make?('workers -> TList('W), 'next -> TFun(TList(TSrv('W)) -> TSrv('W)),'k -> ?(TWorker('A)))) {
        Let('lbWorker, TWorker('A),
          ServerImpl (
            Rule('init?()) {
              Letk(TInternal('A,'B))('spawned, TInstL('W), MapK('W, TSrv('W))!!('workers, Lambda('w,TList('W) -> TInstL('W), Spawn('w)))) {
                     'this~>'insts!!('spawned) && ForEach(TSrv('W))!!('spawned, Lambda('inst, TSrv('W) -> Unit, 'inst~>'init!!()))
                  }
            },
            Rule('work?('thunk -> TThunk('A), 'k -> ?('A)),
                 'insts?('l -> TList(TSrv('W)))) {
              'this~>'insts!!'l && Letk(TInternal('A,'W))('w, TSrv('W), 'next!!'l) { 'w~>'work!!('thunk, 'k)  }
            })) {
          'k!!'lbWorker
        }
      }
    }
  }
}

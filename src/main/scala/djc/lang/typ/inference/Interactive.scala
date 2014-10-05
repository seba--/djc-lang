package djc.lang.typ.inference

import djc.lang.TypedLanguage
import djc.lang.typ.inference.ExtSyntaxDerived._
import djc.lang.base.Bool._
import djc.lang.lib.Function
import djc.lang.lib.combinators.aux.{FilterK, AnyK, ForEach}
import djc.lang.typ.Checker
import djc.lang.typ.inference.TypeInference._
import djc.lang.typ.inference.base.Lists._
import djc.lang.typ.inference.base.Maps._
import djc.lang.typ.inference.base.Pairs._
import djc.lang.base.Integer._
import djc.lang.base.IntegerCompare._
import util.vis.TreeInspector

object Interactive extends App {
  import ExtLanguage._
  import ExtLanguage.types._

  implicit def toExt(e: TypedLanguage.Exp): Exp = e.toFamily(ExtLanguage)

  val TThunk = TUniv('alpha, TSrvRep('$force -> ?(?('alpha))))
  val TWorker = TUniv('alpha, TSrvRep('work -> TFun(TThunk('alpha) -> 'alpha), 'init -> ?()))
  val TStop = TSrvRep('stop -> ?())
  val TStWorker = TUniv('alpha, TWorker('alpha) ++ TStop)
  val TLaWorker = TUniv('alpha, TWorker('alpha) ++ TSrvRep('getLoad -> ?(?(TInteger))))
  val THost = TUniv('alpha, TStop, TSrvRep('host -> ?('alpha, ?(TInteger)), 'resolve -> ?(TInteger, ?(TInteger), ?(TSrv('alpha)))))
  val TMigrate = TUniv('alpha, TStop, TSrvRep('migrate -> ?(TInteger, TSrv(THost('alpha)), ?(TInteger), ?(TInteger))))
  val THostM = TUniv('alpha, TStop, THost('alpha) ++ TMigrate('alpha))

  val TFilterK = TUniv('A, TFun(TList('A), TFun(TVar('A) -> TBool), TList('A)))
  val TFilterKInt = TUniv('A, TSrv(TSrvRep('fk -> TFun(TList('A), TList('A)))))


  val filterK =
    TAbs('A) {
      LocalServer {
        Rule('filterk?('xs -> TList('A), 'fun -> TFun(TVar('A) -> TBool), 'k -> ?(TList('A)))) {
          LocalServer {
            Rule('fk?('xs -> TList('A), 'k -> ?(TList('A)))) {
              Ifc('xs.isNil) {
                'k!!nil
              } Else {
                Letk('keep <-- ('fun !! 'xs.head)) {
                  Letk('ys <-- ('this~>'fk !! 'xs.tail)) {
                    Ifc('keep) {
                      'k!!('xs.head :: 'ys)
                    } Else {
                      'k!!'ys
                    }

                  }
                }
              }
            }
          }~>'fk!!('xs, 'k)
        }
      }~>'filterk
    }


  val TAnyK = TUniv('A, TFun(TList('A), TFun(TVar('A) -> TBool), TBool))
  val TAnyKInt = TUniv('A, TSrv(TSrvRep('ak -> TFun(TList('A), TBool))))

  val anyK =
    TAbs('A) {
      LocalServer {
        Rule('anyk?('xs -> TList('A), 'fun -> TFun(TVar('A) -> TBool), 'k -> ?(TBool))) {
          LocalServer {
            Rule('ak?('xs -> TList('A), 'k -> ?(TBool))) {
              Ifc('xs.isNil) {
                'k!!fal()
              } Else {
                Letk('check <-- ('fun !! 'xs.head)) {
                  Ifc('check) {
                    'k!!tru()
                  } Else {
                    'this~>'ak!!('xs.tail, 'k)
                  }
                }
              }
            }
          }~>'ak!!('xs, 'k)
        }
      }~>'anyk
    }


  val TForEachInt = TUniv('A, TSrv(TSrvRep('foreach -> ?(TList('A)))))
  val TForEach = TUniv('A, ?(TList('A), TFun(TVar('A) -> Unit)))


  val forEach = TAbs('A) {
    LocalServer {
      Rule('foreach?('xs -> TList('A), 'body -> TFun(TVar('A) -> Unit))) {
        LocalServer {
          Rule('foreach?('xs -> TList('A))) {
            Ifc('xs.isNil) {
              Par()
            } Else {
              'body !!('xs.head, Function.consume(Unit)) && 'this ~> 'foreach !! 'xs.tail
            }
          }
        }~>'foreach!!('xs)
      }
    }~>'foreach
  }


  val TP = TPair(TInteger, TInteger, TThunk('A), ?('A))
  val TInternal = TUniv('A, TUniv('W, TWorker('A),
    TSrv(TSrvRep('init -> ?(), 'work -> TFun(TThunk('A) -> 'A), 'inst -> ?(TSrv('W)),  'pending -> ?(TList(TP)), 'done -> ?(TInteger)))))


  val tpe = TUniv('A, TUniv('W, TStWorker('A), TSrvRep('make -> ?('W, TInteger, ?(TWorker('A))))))
  val impl = TAbs('A, 'W << TStWorker('A)) {
    ServerImpl {
      Rule('make?('worker -> 'W, 'timeout -> TInteger, 'k -> ?(TWorker('A)))) {
        Let('selfrecovering <--
          ServerImpl (
            Rule('init?()) {
              Let('w <-- SpawnLocal('worker)) {
                'w~>'init!!() && 'this~>'inst!!'w && 'this~>'pending!!nil
              }
            },
            Rule('work?('thunk -> TThunk('A), 'k -> ?('A)),
              'inst?('w -> TSrv('W)), 'pending?('xs -> TList(TP))) {
              Let('id <-- freshInt()) {
                ('this~>'pending!!(pair('id, localTime(), 'thunk, 'k) :: 'xs.l)
                  && 'this~>'inst!!'w
                  && Letk('r <-- ('w~>'work!!'thunk)) {
                  'k!!'r && 'this~>'done!!'id
                }
                  )
              }
            },
            Rule('done?('id -> TInteger), 'pending?('xs -> TList(TP))) {
              filterK(TP)!!('xs, Lambda('p)('p.fst <> 'id), 'this~>'pending)
            },
            Rule('pending?('xs -> TList(TP)),'inst?('w -> TSrv('W))) {
              Let('time <-- localTime()) {
                Let('work <-- 'this~>'work) {
                  Letk('late <-- (anyK(TP) !!('xs, Lambda('p)(('time - 'p.snd) > 'timeout)))) {
                    Ifc('late) {
                      'w ~> 'stop !!() && 'this ~> 'init !!() && forEach(TP) !!('xs, Lambda('p)('work!!('p.thrd, 'p.frth)))
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

  val impl2 = TAbs('A, 'W << TStWorker('A)) {
    ServerImpl {
      Rule('make ?('worker -> 'W, 'timeout -> TInteger, 'k -> ?(TWorker('A)))) {
        'k!!ServerImpl(
          Rule('init ?()) {
            Let('w <-- SpawnLocal('worker)) {
              'w ~> 'init !!() && 'this ~> 'inst !! 'w && 'this ~> 'pending !! nil
            }
          },
          Rule('work ?('thunk -> TThunk('A), 'k -> ?('A)),
            'inst ? ('w -> TSrv('W)), 'pending ? ('xs -> TList(TP))) {
            Let('id <-- freshInt()) {
              ('this~>'pending!!(pair('id, localTime(), 'thunk, 'k) :: 'xs.l)
                && 'this~>'inst!!'w
                && Letk('r <-- ('w~>'work!!'thunk)) {
                'k!!'r && 'this~>'done!!'id
              }
                )
            }
          },
          Rule('done ? ('id -> TInteger), 'pending ? ('xs -> TList(TP))) {
            filterK(TP)!!('xs, Lambda('p)('p.fst <> 'id), 'this~>'pending)
          },
          Rule('pending ? ('xs -> TList(TP)), 'inst ? ('w -> TSrv('W))) {
            Ifc(tru) {
              'w ~> 'stop !!()
            } Else {
              'this ~> 'inst !! 'w
            }
          })
      }
    }
  }



  try {
    TreeInspector(impl, "Before")
    val implinferred = infer(ProtoTypes.Hole, Map(), Map(), impl)
    TreeInspector(implinferred, "After")
    val original = Checker.typeCheck(Map(), Map(), djc.lang.lib.combinators.recovery.MkRecover.impl)
    val inferred = implinferred._1
    val checked = Checker.typeCheck(Map(), Map(), implinferred._2)
    TreeInspector(Map("original" -> original,
                      "inferred" -> implinferred._1,
                      "checked"  -> checked), "Types")
    println(original === inferred)
    println(checked === inferred)
  }
  catch {
    case e: Exception =>

      TreeInspector(List(e, e.getStackTraceString), "Exception")
  }
}

package djc.lang.lib.combinators.migration

import djc.lang.TypedSyntax._
import djc.lang.TypedSyntaxDerived.{TThunk => _, _}
import djc.lang.base.Pairs._
import djc.lang.base.Maps._
import djc.lang.base.Integer._
import djc.lang.lib.combinators._
import djc.lang.typ.Types._

object MkHost {
  def apply(t1: Type) = TApp(combinator, t1)

  val TP = TTuple('A, TSrv('A))
  val TM = TMap(TInteger, TP)
  val TInternal = TUniv('A, TStop, THostM('A) ++ TSrvRep('vms -> ?(TM)))

  val combinator = TAbs('A << TStop) {
    ServerImpl {
      Rule('make?('k -> ?(THostM('A)))) {
        Let('host, THostM('A),
          ServerImpl (
            Rule('host?('image -> 'A, 'k -> ?(TInteger)), 'vms?('map -> TM)) {
              Let(TInternal('A))('id, TInteger, freshInt()) {
                Let('inst, TSrv('A), Spawn('image)) {
                  'this~>'vms!!('map.insert(TInteger, TP, Var('id) -> 'inst)) && 'k!!'id
                }
              }
            },
            Rule('resolve?('id -> TInteger, 'fail -> ?(TInteger), 'k -> ?(TSrv('A))), 'vms?('map -> TM)) {
                  'this~>'vms!!'map && (Ifc('map.hasKey(TInteger, TP, 'id)) {
                                         'k!!(snd(TP, 'map.get(TInteger, TP,'id)))
                                        } Else {
                                          'fail!!'id
                                        })
            },
            Rule('migrate?('id -> TInteger, 'host -> THost('A), 'fail -> ?(TInteger), 'k -> ?(TInteger)), 'vms?('map -> TM)) {
              Ifc(TInternal('A))('map.hasKey(TInteger, TP, 'id)) {
                Let('p, TP, 'map.get(TInteger, TP, 'id)) {
                  snd(TP, 'p)~>'stop!!() && 'this~>'vms!!('map.remove(TInteger, TP, 'id)) && 'host~>'host!!(fst(TP, 'p), 'k)
                }
              } Else {
                'fail!!'id && 'this~>'vms!!'map
              }
            }
          )) {
          'k!!'host
        }
      }
    }
  }
}


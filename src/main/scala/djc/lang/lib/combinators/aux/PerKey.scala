package djc.lang.lib.combinators.aux

import djc.lang.TypedLanguage._
import djc.lang.TypedLanguage.types.{TPair => _, _}
import djc.lang.TypedSyntaxDerived._
import djc.lang.base.Lists._
import djc.lang.base.Maps._
import djc.lang.base.Pairs._
import djc.lang.lib.combinators.Combinator


/**
  * Created by oliver on 04.08.16.
  */
object PerKey extends Combinator {
  def apply(t: Type, t2: Type) = TApp(impl, t, t2)

  val P = TPair('A, 'B)
  val LP = TList(P)
  val MAC = TMap('A, 'C)
  val FAC = TFun(TVar('B), TVar('C))

  val foldfun =
    KLambda(List('m -> MAC, 'p -> P),
      Ifc('m.hasKey('A, 'C, 'p.fst(P))) {
        'k !! ('m)
      } Else {
        Letk('v, 'C, 'fun !! ('p.snd(P))) {
          'k !! ('m.insert(TVar('A), TVar('C), ('p.fst(P), 'v)))
        }
      }, 'k, MAC)

  val TPerKey = TUniv('A, TUniv('B, TUniv('C, TFun(LP, FAC, MAC))))

  val tpe = TPerKey
  val impl =
    TAbs('A, 'B, 'C) {
      LocalServer {
        Rule('perkey?('xs -> LP, 'fun -> FAC, 'k -> ?(MAC))) {
          FoldK(P, MAC) !! ('xs, empty('A, 'C), foldfun ,'k)
        }
      }~>'perkey
    }

}
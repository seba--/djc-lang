package djc.lang.lib

import djc.lang.TypedLanguage._
import djc.lang.TypedLanguage.types._
import djc.lang.TypedSyntaxDerived.{TThunk => _, _}
import djc.lang.base.Integer._


package object combinators {
  val TThunk = TUniv('alpha, TSrvRep('$force -> ?(?('alpha))))
  val TWorker = TUniv('alpha, TSrvRep('work -> TFun(TThunk('alpha) -> 'alpha), 'init -> ?()))
  val TStop = TSrvRep('stop -> ?())
  val TStWorker = TUniv('alpha, TWorker('alpha) ++ TStop)
  val TLaWorker = TUniv('alpha, TWorker('alpha) ++ TSrvRep('getLoad -> ?(?(TInteger))))
  val THost = TUniv('alpha, TStop, TSrvRep('host -> ?('alpha, ?(TInteger)), 'resolve -> ?(TInteger, ?(TInteger), ?(TSrv('alpha)))))
  val TMigrate = TUniv('alpha, TStop, TSrvRep('migrate -> ?(TInteger, TSrv(THost('alpha)), ?(TInteger), ?(TInteger))))
  val THostM = TUniv('alpha, TStop, THost('alpha) ++ TMigrate('alpha))
}

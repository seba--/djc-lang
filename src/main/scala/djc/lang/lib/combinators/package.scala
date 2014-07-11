package djc.lang.lib

import djc.lang.typ.Types._
import djc.lang.TypedSyntax._
import djc.lang.TypedSyntaxDerived.{TThunk => _, _}
import djc.lang.base.Integer._


package object combinators {
  val TThunk = TUniv('alpha, TSrvRep('$force -> ?(?('alpha))))
  val TWorker = TUniv('alpha, TSrvRep('work -> TFun(TThunk('alpha) -> 'alpha), 'init -> ?()))
  val TStop = TSrvRep('stop -> ?())
  val TStWorker = TUniv('alpha, TWorker('alpha) ++ TStop)
  val TLaWorker = TUniv('alpha, TWorker('alpha) ++ TSrvRep('getLoad -> ?(?(TInteger))))
  val THost = TUniv('alpha, Some(TStop), TSrvRep('host -> ?('alpha, ?(TInteger)), 'resolve -> ?(TInteger, ?(TInteger), ?(TSrv('alpha)))))
  val TMigrate = TUniv('alpha, Some(TStop), TSrvRep('migrate -> ?(TInteger, TSrv(THost('alpha)), ?(TInteger), ?(TInteger))))
}

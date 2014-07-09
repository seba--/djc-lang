package djc.lang.lib.autonomous

import djc.lang.TypedSyntax._
import djc.lang.TypedSyntaxDerived._
import djc.lang.typ.Types._
import djc.lang.base.Bool._
import djc.lang.lib.autonomous.Sensor._

object FrontDoor {

  val TDoorLock = TSrvRep('init -> ?(), 'lock -> ?(), 'unlock -> ?(), 'isLocked -> ?(?(TBool)))
  val DoorLock =
    ServerImpl(
      Rule('init?(), 'this~>'lockState!!(fal)),
      Rule('lock?() && 'lockState?('b -> TBool), 'this~>'lockState!!(tru)),
      Rule('unlock?() && 'lockState?('b -> TBool), 'this~>'lockState!!(fal)),
      Rule('isLocked?('k -> ?(TBool)) && 'lockState?('b -> TBool), 'k!!('b))
    ) as TDoorLock


  val TIntruderSensor = TSensor(TBool)
  val IntruderSensor = makeSensor(TBool, fal, Map(0.05 -> tru))

  val IntruderReaction =
    ServerImpl(
      Rule(
        'state?('s -> TIntruderSensor, 'd -> TDoorLock),
        Letk('intruderDetected, TBool, 's~>'sense!!()) {
          Ifc('intruderDetected) {
            'd~>'lock!!() && 'this~>'state!!('s, 'd)
          }.
          Else {
            'this~>'state!!('s, 'd)
          }
        })
    )
}
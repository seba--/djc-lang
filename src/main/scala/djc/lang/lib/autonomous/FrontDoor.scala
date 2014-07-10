  package djc.lang.lib.autonomous

import djc.lang.TypedSyntax._
import djc.lang.TypedSyntaxDerived._
import djc.lang.typ.Types._
import djc.lang.base.Bool._
import djc.lang.base.String._
import djc.lang.lib.autonomous.Sensor._
  import djc.lang.typ.Checker

  object FrontDoor {


  val TDoorLock = TSrv(TSrvRep('init -> ?(), 'lock -> ?(), 'unlock -> ?(), 'isLocked -> ?(?(TBool))))
  val _TDoorLock = TSrv(TSrvRep('init -> ?(), 'lock -> ?(), 'unlock -> ?(), 'isLocked -> ?(?(TBool)), 'lockState -> ?(TBool)))
  val DoorLock =
    ServerImpl(
      Rule('init?(), 'this~>'lockState!!(fal)),
      Rule(
        'lock?() && 'lockState?('b -> TBool),
        Let('self, _TDoorLock, 'this) {
          print("door locked",
            LocalService('k?(), 'self~>'lockState!!(tru)))}),
      Rule(
        'unlock?() && 'lockState?('b -> TBool),
        Let('self, _TDoorLock, 'this) {
          print("door unlocked",
            LocalService('k?(), 'self~>'lockState!!(fal)))}),
      Rule(
        'isLocked?('k -> ?(TBool)) && 'lockState?('b -> TBool),
        'k!!('b))
    ) as TSrvRep('init -> ?(), 'lock -> ?(), 'unlock -> ?(), 'isLocked -> ?(?(TBool)))


  val TIntruderSensor = TSensor(TBool)
  val IntruderSensor = makeSensor(TBool, fal, Map(0.05 -> tru))

  val TIntruderReaction = TSrv(TSrvRep('state -> ?(TIntruderSensor, TDoorLock)))
  val IntruderReaction =
    ServerImpl(
      Rule(
        'state?('s -> TIntruderSensor, 'd -> TDoorLock),
        Letk(TIntruderReaction)('intruderDetected, TBool, 's~>'sense!!()) {
          Ifc(TIntruderReaction)('intruderDetected) {
            'd~>'lock!!() && 'this~>'state!!('s, 'd)
          }.
          Else {
            'this~>'state!!('s, 'd)
          }
        })
    )

  val system =
    Let('doorlock, TDoorLock, Spawn(DoorLock)) {
    Let('intrudersensor, TIntruderSensor, Spawn(IntruderSensor)) {
    Let('intruderreaction, TIntruderReaction, Spawn(IntruderReaction)) {
      'doorlock~>'init!!() && 'intruderreaction~>'state!!('intrudersensor, 'doorlock)
    }}}

  def checkSystem = Checker.typeCheck(Map(), Set(), system)
  def runSystem = djc.lang.sem.concurrent_6_thread.SemanticsFactory.newInstance().interp(Par(system).eraseType)

}
package djc.lang.lib.autonomous

import djc.lang.typ.Types._
import djc.lang.TypedSyntax._
import djc.lang.TypedSyntaxDerived._
import djc.lang.base.Double._
import djc.lang.base.DoubleCompare._
import djc.lang.base.String._

object Sensor {

  val TSensor = TUniv('alpha, TSrv(TSrvRep('sense -> ?(?('alpha)))))

  def makeSensor(sensorType: Type, default: Exp, dist: Map[Double, Exp]): Exp =
    ServerImpl(
      Rule(
        'sense?('k -> ?(sensorType)),
        Let('rnd, TDouble, NextRandom) {
          makeDistExp('k, 'rnd, default, dist)
        }
      )
    )

  def makeDistExp(k: Exp, rnd: Exp, default: Exp, dist: Map[Double, Exp]): Exp = {
    val sum = dist.keys.sum
    val normDist = dist map (kv => (kv._1 + sum, kv._2))
    val sortedNormDist = normDist.toList.sortBy(p => -p._1)
    var e: Exp = k!!(default)
    for ((d,v) <- sortedNormDist)
      e = Ifc(rnd <== d)(k!!(v))Else(e)
    e
  }

  val TSensorLogger = TUniv('alpha, TSrv(TSrvRep('log -> ?(TSensor('alpha)))))
  val sensorLogger = TAbs('alpha,
    ServerImpl(
      Rule(
        'log?('s -> TSensor('alpha)),
        Letk(TSensorLogger('alpha))('v, 'alpha, 's~>'sense!!()) {
          'this~>'log!!('s) &&
          print('v.asString, LocalService('k?(), Par()))
        }
      )
    )
  )

  def testSensor(t: Type, sensorImpl: Exp) =
    Spawn(sensorLogger(t))~>'log!!(Spawn(sensorImpl))
}
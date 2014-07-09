package djc.lang.lib.autonomous

import djc.lang.typ.Types._
import djc.lang.TypedSyntax._
import djc.lang.TypedSyntaxDerived._
import djc.lang.base.Double._
import djc.lang.base.DoubleCompare._

object Appliance {

   val TAppliance = TUniv('alpha, TSrv(TSrvRep('sense -> ?(?('alpha)))))

   def makeSensor(sensorType: Type, default: Exp, dist: Map[Double, Exp]): Exp =
     ServerImpl(
       Rule(
         'sensor?('k -> sensorType),
         Let('rnd, TDouble, NextRandom) {
           makeDistExp('rnd, default, dist)
         }
       )
     )

   def makeDistExp(rnd: Exp, default: Exp, dist: Map[Double, Exp]): Exp = {
     val sum = dist.keys.sum
     val normDist = dist map (kv => (kv._1 + sum, kv._2))
     val sortedNormDist = dist.toList.sortBy(p => -p._1)
     var e = default
     for ((k,v) <- sortedNormDist)
       e = Ifc(k <== rnd)(v)Else(e)
     e
   }
 }
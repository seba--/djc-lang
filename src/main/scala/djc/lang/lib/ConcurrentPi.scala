package djc.lang.lib

import djc.lang.TypedSyntax._
import djc.lang.TypedSyntaxDerived._
import djc.lang.typ.Types._
import djc.lang.base.Double._
import djc.lang.base.Integer._
import djc.lang.base.IntegerCompare._
import djc.lang.base.Bool._


/**
 * Created by seba on 06/06/14.
 */
object ConcurrentPi {

  val plus = Lambda(List('x -> TDouble, 'y -> TDouble), BaseCall(djc.lang.base.Double.Plus, Var('x), Var('y)), TDouble)

  val formulaType = TFun(TInteger, TDouble)
  val formula = Lambda('i, TInteger, (4.0 * ((-1.0 pow Var('f)) / (2.0 * Var('f) + 1.0))), TDouble)


  val forType = ?(TInteger, //init
                  TFun(TInteger, TBool), // continuation condition
                  TFun(TInteger, TInteger), // counter modifier
                  TFun(TInteger, Unit)) // body
  val forService = LocalService(
    'for?('current -> TInteger, 'cond -> TFun(TInteger, TBool), 'mod -> TFun(TInteger, TInteger), 'body -> TFun(TInteger, Unit)),
    Def('outer, TSrv(TSrvRep('for -> forType)), 'this,
      'cond!!('current,
        LocalService(
          'kfor?('shouldContinue -> TBool),
          Ifc('shouldContinue,
            'body!!('current, Function.consume(Unit)) && 'mod!!('current, LocalService('k?('next -> TInteger), 'outer~>'for!!('next, 'cond, 'mod, 'body))),
            Par()
          )
        ))
    )
  )

  val reducerType = TSrv(TSrvRep('reduce -> ?(TDouble), 'waitFor -> ?(TInteger)))
  val mkReducer =
    Server(Rule('make?('numResults -> TInteger, 'finalK -> ?(TDouble), 'op -> TFun(TDouble, TDouble, TDouble), 'kred -> ?(reducerType)),
      Def('res, reducerType,
        Server(
          Rule(
            'waitFor?('n -> TInteger) && 'reduce?('v1 -> TDouble) && 'reduce?('v2 -> TDouble),
            'this~>'waitFor!!('n - 1) && 'op!!('v1, 'v2, 'this~>'reduce)
          ),
          Rule(
            'waitFor?('n -> TInteger) && 'reduce?('v -> TDouble),
            Def('reducer, TSrv(TSrvRep('reduce -> ?(TDouble), 'waitFor -> ?(TInteger))), 'this,
              Ifc('n <== 1,
                'finalK!!('v),
                'reducer~>'waitFor!!('n) && 'reducer~>'reduce!!('v)
              )
            )
          )
        ),
        'res~>'waitFor!!('numResults) && 'kred!!('res~>'reduce)
      )
    ))

  val mapperType = TSrv(TSrvRep( 'map -> ?(TInteger)  ))
  val mkMapper =
    Server(Rule('make?('reducer -> reducerType, 'f -> TFun(TInteger, TDouble), 'k -> ?(mapperType)),
      'k!!Server(Rule('map?('i -> TInteger), 'f!!('i, 'reducer~>'reduce)))
    ))

  val piServerType = TSrvRep('pi -> TFun(TInteger, TDouble))

  val piServer = ServerImpl(
    Rule(
      'pi?('n -> TInteger, 'k -> ?(TDouble)),
      Defk('reducer, reducerType, mkReducer~>'make!!('n.i + 1, plus, 'k),
        Defk('mapper, mapperType, mkMapper~>'make!!('reducer, formula),
          forService!!(
            0,
            Lambda('i, TInteger->TBool, 'i <== 'n),
            Lambda('i, TInteger->TInteger, 'i.i + 1),
            Lambda('i, TInteger->Unit, 'mapper!!('i.toDouble))
          )
        )
      )
    )
  )


  def concurrentPi(n: Int): Double = {
    var result = 0.0
    for (i <- 0 to n)
      result += 4.0 * (Math.pow(-1, i) / (2.0 * i + 1.0))
    result
  }

  // GO example: https://github.com/foamdino/learning-go/blob/master/concurrent-pi/concurrent-pi.go
  /*
  package main

  import (
    "fmt"
    "math"
  )

  func formula(ch chan float64, term float64) {
    result := 4 * (math.Pow(-1, term) / (2.0 * term + 1.0))
    ch <- result
  }

  func calc_pi(term int) float64 {
    ch := make(chan float64)

    //map
    for i := 0; i <= term; i++ {
      go formula(ch, float64(i))
    }

    result := 0.0

    //reduce
    for i := 0; i <= term; i++ {
      result += <- ch
    }

    return result
  }

  func main() {
    fmt.Println(calc_pi(300))
  }
   */
}

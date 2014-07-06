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

  val formulaType = TFun(TDouble, TDouble)
  val formula = Lambda('i, TDouble->TDouble, (4.0 * ((-1.0 pow Var('i)) / (2.0 * Var('i) + 1.0))))


  val forType = ?(TInteger, //init
                  TFun(TInteger, TBool), // continuation condition
                  TFun(TInteger, TInteger), // counter modifier
                  TFun(TInteger, Unit)) // body
  val forService = LocalService(
    'for?('current -> TInteger, 'cond -> TFun(TInteger, TBool), 'mod -> TFun(TInteger, TInteger), 'body -> TFun(TInteger, Unit)),
    This(TSrv(TSrvRep('for -> forType))) {
      'cond !!('current,
        LocalService(
          '$kfor ? ('shouldContinue -> TBool),
          Ifc(Var('shouldContinue)) {
            'body !!('current, Function.consume(Unit)) && 'mod !!('current, LocalService('$k ? ('next -> TInteger), 'this ~> 'for !!('next, 'cond, 'mod, 'body)))
          } Else {
            Par()
          }
        ))
    }
  )

  def reducerType(tpe: Type) = TSrv(TSrvRep('reduce -> ?(tpe), 'wait -> ?(TInteger)))
  val mkReducerType = TUniv('A, TSrv(TSrvRep('make -> ?(TInteger, ?('A), TFun('A, 'A, 'A), ?(reducerType('A))))))
  val mkReducer =
    TAbs('A ,LocalServer(Rule('make?('numResults -> TInteger, 'finalK -> ?('A), 'op -> TFun('A, 'A, 'A), 'kred -> ?(reducerType('A))),
      Let('res, reducerType('A),
        Server(
          Rule(
            'wait?('n -> TInteger) && 'reduce?('v1 -> 'A) && 'reduce?('v2 -> 'A),
            'this~>'wait!!('n - 1) && 'op!!('v1, 'v2, 'this~>'reduce)
          ),
          Rule(
            'wait?('n -> TInteger) && 'reduce?('v -> 'A),
            Let('reducer, reducerType('A), 'this) {
              Ifc('n <== 0) {
                'finalK !! ('v)
              } Else {
                'reducer ~> 'wait !! ('n) && 'reducer ~> 'reduce !! ('v)
              }
            }
          )
        )) {
        'res ~> 'wait !! ('numResults) && 'kred !! ('res)
       }
    )))

  def mapperType(tpe: Type) = TSrv(TSrvRep( 'map -> ?(tpe)  ))
  val mkMapper =
    TAbs('A, TAbs('B, LocalServer(Rule('make?('reducer -> reducerType('B), 'f -> TFun('A, 'B), 'k -> ?(mapperType('A))),
      'k!!Server(Rule('map?('i -> 'A), 'f!!('i, 'reducer~>'reduce)))
    ))))

  val piServerType = TSrvRep('pi -> TFun(TInteger, TDouble))
  val piServer = ServerImpl(
    Rule(
      'pi?('n -> TInteger, 'k -> ?(TDouble)),
      Let('summand, TFun(TDouble,TDouble), formula)(
      Letk('reducer, reducerType(TDouble), TApp(mkReducer, TDouble)~>'make!!('n, 'k, plus))(
        Letk('mapper, mapperType(TDouble), TApp(mkMapper, TDouble, TDouble)~>'make!!('reducer, 'summand))(
          forService!!(
            0,
            Lambda('i, TInteger->TBool, 'i <== 'n),
            Lambda('i, TInteger->TInteger, 'i.i + 1),
            Lambda('i, TInteger->Unit, 'mapper~>'map!!('i.toDouble))
          )
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

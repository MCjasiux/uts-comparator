package agh

import agh.Surface._
class H2h(_name:String, _prediction:Array[Float],_elos:Array[String],_surfaceWins:Map[Surface,Array[Float]],_bets:Array[Int]){
     def name = _name
     def prediction = _prediction
     def elos = _elos
     def surfaceWins = _surfaceWins
     def bets = _bets
}
package agh

import agh.Surface.Clay
import Array._
import agh.ScalaFXHelloWorld
//import agh.Completer._

object Main extends App {
  val comparator = new Comparator()
  var output:String = " "

  val gui = ScalaFXHelloWorld
  //comparator.compare(Array(4920,4913),Clay) //djokovic i murray
  comparator.compareGMM(Array(644,4742, 4920, 3018, 3819), Clay) //djokovic, murray, nadal, federer

}

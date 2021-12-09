package agh

import agh.Surface.Clay

object Main extends App {
  val comparator = new Comparator()
  //comparator.compare(Array(4920,4913),Clay) //djokovic i murray
  comparator.compareGMM(Array(4920, 4913, 4742, 3819), Clay) //djokovic, murray, nadal, federer
}

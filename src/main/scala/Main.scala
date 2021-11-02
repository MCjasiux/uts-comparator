package agh
import scala.io.Source

object Main extends App {
  val fetcher = new Fetcher()
  fetcher.fetch(27834,4913)

}
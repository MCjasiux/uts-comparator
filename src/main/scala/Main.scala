package agh
import org.jsoup.Jsoup
import scala.io.Source

object Main extends App {
  
  val html = Source.fromURL("https://www.ultimatetennisstatistics.com/h2hProfiles?playerId1=27834&playerId2=4913")
    val str = html.mkString
    val document = Jsoup.parse(str)
    val names = Array(document.select("a").get(0).text(),document.select("a").get(0).text())
    println("tytuły:" + document.select("a.player-title").get(0).text())
}
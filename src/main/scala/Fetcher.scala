package agh
import scala.io.Source
import org.jsoup.Jsoup

    class Fetcher() {
        def fetch(id1:Int,id2:Int){
            val html = Source.fromURL("https://www.ultimatetennisstatistics.com/h2hProfiles?playerId1="+id1+"&playerId2="+id2)
            val str = html.mkString
            val document = Jsoup.parse(str)
            val names = Array(document.select("a").get(0).text(),document.select("a").get(1).text())  //imiona tenisistów
            val titles = Array(document.select("a.player-title").get(0).text(),document.select("a.player-title").get(1).text()) //liczba tytułów
            val ranks = Array(document.select("td.text-right").get(25).text(),document.select("td.text-left").get(25).text())
            val elos = Array(document.select("td.text-right").get(27).text(),document.select("td.text-left").get(27).text())
            println(names(0)," : "+names(1))
            println(titles(0)," : "+titles(1))
            println(ranks(0)," : "+ranks(1))
            println(elos(0)," : "+elos(1))

        }
}
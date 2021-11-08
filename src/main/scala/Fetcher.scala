package agh
import scala.io.Source
import org.jsoup.Jsoup
import scala.util.matching.Regex
import agh.Surface._
    class Fetcher() {
        def fetch(id1:Int,id2:Int){
            //var html:Option[scala.io.BufferedSource] = None
            
            try{
            val html = Source.fromURL("https://www.ultimatetennisstatistics.com/h2hProfiles?playerId1="+id1+"&playerId2="+id2)
            val str = html.mkString
            val document = Jsoup.parse(str)


            val names = Array(document.select("a").get(0).text(),document.select("a").get(1).text())  //imiona tenisistów

            val pattern1 = new Regex("\\(\\d*?\\)")
            val pattern2 = new Regex("\\d*?\\.\\d*?\\%")

            // val elos = Array(
            //     (pattern1 findAllIn document.select("td.text-right").get(27).text()).mkString("").stripPrefix("(").stripSuffix(")").toInt,
            //     (pattern1 findAllIn document.select("td.text-left").get(27).text()).mkString("").stripPrefix("(").stripSuffix(")").toInt
            //     )


            val surfaceAdjusted = Array(
                (pattern2 findAllIn document.select("[title='Show H2H matches']").get(0).text()).mkString("").stripSuffix("%").toFloat,
                (pattern2 findAllIn document.select("[title='Show H2H matches']").get(1).text()).mkString("").stripSuffix("%").toFloat
                )
            
            val clayMatches = Array(
                (pattern2 findAllIn document.select("[title='Show clay matches']").get(0).text()).mkString("").stripSuffix("%").toFloat,
                (pattern2 findAllIn document.select("[title='Show clay matches']").get(1).text()).mkString("").stripSuffix("%").toFloat
            )
            val hardMatches = Array(
                (pattern2 findAllIn document.select("[title='Show hard matches']").get(0).text()).mkString("").stripSuffix("%").toFloat,
                (pattern2 findAllIn document.select("[title='Show hard matches']").get(1).text()).mkString("").stripSuffix("%").toFloat
            )
            val carpetMatches = Array(
                (pattern2 findAllIn document.select("[title='Show carpet matches']").get(0).text()).mkString("").stripSuffix("%").toFloat,
                (pattern2 findAllIn document.select("[title='Show carpet matches']").get(1).text()).mkString("").stripSuffix("%").toFloat
            )
            val grassMatches = Array(
                (pattern2 findAllIn document.select("[title='Show grass matches']").get(0).text()).mkString("").stripSuffix("%").toFloat,
                (pattern2 findAllIn document.select("[title='Show grass matches']").get(1).text()).mkString("").stripSuffix("%").toFloat
            )

            println(names(0)," : "+names(1))        //pełne imiona tenisistów
           // println(elos(0)," : "+elos(1))          //punkty elo
            println(surfaceAdjusted(0),surfaceAdjusted(1))  //surface adjusted h2h [%]
            println(clayMatches(0),clayMatches(1))     

          //  val h2h = new H2h(names(0)+" vs "+names(1),)
            }catch{
                case ex:Exception =>{
                    println("Error fetching data")
                    println(ex)
                }
            }


        }
}
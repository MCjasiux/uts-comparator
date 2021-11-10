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

            /*
            Niektórzy tenisiści brali udział w Pucharach Davisa, dzięki czemu w tabeli porównawczej jest
            więcej wierszy. Regex próbuje wydostać liczbę z oczekiwanej wartości z trzech komórek z otoczenia trzech wierszy
            w przypadku przesunięcia wartość będzie pusta, bo spróbuje pobrać datę zamiast liczby
            */
            val textLeft= document.select("td.text-right")
            val textRight = document.select("td.text-left")
            val elos = Array("","")
            var i =0
            for( i <- 1 to 3){
                elos(0)+= (pattern1 findAllIn textLeft.get(26+i).text()).mkString("").stripPrefix("(").stripSuffix(")")
                elos(1)+= (pattern1 findAllIn textRight.get(26+i).text()).mkString("").stripPrefix("(").stripSuffix(")")
            }
            
            // val elos = Array(
            //     (pattern1 findAllIn textLeft.get(27).text()).mkString("").stripPrefix("(").stripSuffix(")").toInt,
            //     (pattern1 findAllIn textRight.get(27).text()).mkString("").stripPrefix("(").stripSuffix(")").toInt
            //     )


            val surfaceAdjusted = Array(
                (pattern2 findAllIn document.select("[title='Show H2H matches']").get(0).text()).mkString("").stripSuffix("%").toFloat,
                (pattern2 findAllIn document.select("[title='Show H2H matches']").get(1).text()).mkString("").stripSuffix("%").toFloat
                )
            
            val clayMatches:Array[Float] = Array(
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
            println(elos(0)," : "+elos(1))          //punkty elo
            println(surfaceAdjusted(0),surfaceAdjusted(1))  //surface adjusted h2h [%]
            println(clayMatches(0),clayMatches(1))     //%wygranych na danej powierzchni

            val surfaceStats =  Map(
                Clay -> clayMatches,
                Hard ->  hardMatches,
                Carpet -> carpetMatches,
                Grass -> grassMatches 
            )


            val out = new H2h(
                names(0)+" vs " + names(1),
                surfaceAdjusted,
                elos,
                surfaceStats
            )

            }catch{
                case ex:Exception =>{
                    println("Error fetching data")
                    println(ex)
                }
            }


        }
}
package agh
import scala.io.Source
import org.jsoup.Jsoup
import scala.util.matching.Regex
import agh.Surface._
    class Fetcher() {
        def fetch(id1:Int,id2:Int): Any={
            //var html:Option[scala.io.BufferedSource] = None
            var out: Any = 0
            try{
            val html = Source.fromURL("https://www.ultimatetennisstatistics.com/h2hProfiles?playerId1="+id1+"&playerId2="+id2)
            val str = html.mkString
            val document = Jsoup.parse(str)


            val names = Array(document.select("a").get(0).text(),document.select("a").get(1).text())  //imiona tenisistów
            println(names(0)," : "+names(1))        //pełne imiona tenisistów
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
            println(elos(0)," : "+elos(1))          //punkty elo

            // val elos = Array(
            //     (pattern1 findAllIn textLeft.get(27).text()).mkString("").stripPrefix("(").stripSuffix(")").toInt,
            //     (pattern1 findAllIn textRight.get(27).text()).mkString("").stripPrefix("(").stripSuffix(")").toInt
            //     )
            //var surfaceAdjusted:Array[Float] = Array(50,50)
            
            val surfaceAdjusted = Array(
                (pattern2 findAllIn document.select("[title='Show H2H matches']").get(0).text()).mkString("").stripSuffix("%").toFloat,
                (pattern2 findAllIn document.select("[title='Show H2H matches']").get(1).text()).mkString("").stripSuffix("%").toFloat
                )
            

            println(surfaceAdjusted(0),surfaceAdjusted(1))  //surface adjusted h2h [%]

            var clayMatches:Array[Float] = Array(50,50)
            try{
                    clayMatches = Array(
                    (pattern2 findAllIn document.select("[title='Show clay matches']").get(0).text()).mkString("").stripSuffix("%").toFloat,
                    (pattern2 findAllIn document.select("[title='Show clay matches']").get(1).text()).mkString("").stripSuffix("%").toFloat
                )
            }catch{
                case ex:Exception =>{
                    println("One or more players has no record of clay matches, assuming 50% win rate")
                    println(ex)
                    None
                }
            }
            var hardMatches:Array[Float] = Array(50,50)
            try{
                    hardMatches = Array(
                    (pattern2 findAllIn document.select("[title='Show hard matches']").get(0).text()).mkString("").stripSuffix("%").toFloat,
                    (pattern2 findAllIn document.select("[title='Show hard matches']").get(1).text()).mkString("").stripSuffix("%").toFloat
                )
            }catch{
                case ex:Exception =>{
                    println("One or more players has no record of hard matches, assuming 50% win rate")
                    println(ex)
                    None
                }
            }
            var carpetMatches:Array[Float] = Array(50,50)
            try{
                carpetMatches = Array(
                (pattern2 findAllIn document.select("[title='Show carpet matches']").get(0).text()).mkString("").stripSuffix("%").toFloat,
                (pattern2 findAllIn document.select("[title='Show carpet matches']").get(1).text()).mkString("").stripSuffix("%").toFloat
            )
            }catch{
                    case ex:Exception =>{
                    println("One or more players has no record of carpet matches, assuming 50% win rate")
                    println(ex)
                    None
                }
            }
            var grassMatches:Array[Float] = Array(50,50)
            try{
            grassMatches = Array(
                (pattern2 findAllIn document.select("[title='Show grass matches']").get(0).text()).mkString("").stripSuffix("%").toFloat,
                (pattern2 findAllIn document.select("[title='Show grass matches']").get(1).text()).mkString("").stripSuffix("%").toFloat
            )
            }catch{
                    case ex:Exception =>{
                    println("One or more players has no record of grass matches, assuming 50% win rate")
                    println(ex)
                    None
                }
            }

        try{
            val betHTML = Source.fromURL("https://matchstat.com/tennis/h2h-odds-bets/"+names(0)+"/"+names(1))
            val betDocument  = Jsoup.parse(betHTML.mkString)

            val bets = Array(betDocument.select("span.betrating").get(0).text(),betDocument.select("span.betrating").get(2).text())  //wyniki zakładów
            if(bets(0)==""){    //bez danych zakładów
                bets(0)="0"
            }
            if(bets(1)==""){
                bets(1)="0"
            }
            println(bets(0)," : "+bets(1))          //punkty elo

            val surfaceStats =  Map(
                Clay -> clayMatches,
                Hard ->  hardMatches,
                Carpet -> carpetMatches,
                Grass -> grassMatches 
            )

            out = new H2h(
                names(0)+" vs " + names(1),
                surfaceAdjusted,
                elos,
                surfaceStats,
                Array(bets(0).toInt,bets(1).toInt)
            )
        }catch{
                case ex:Exception =>{
                    println("Error fetching bets data")
                    println(ex)
                    None
                }  
        }





            }catch{
                case ex:Exception =>{
                    println("Error fetching data")
                    println(ex)
                    None
                }
            }
            finally {
                return out
            }


        }
}
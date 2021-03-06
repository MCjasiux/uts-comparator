package agh

import javax.net.ssl._
import java.security.cert.X509Certificate

import agh.Surface._
import org.jsoup.Jsoup
import scala.io.Source
import scala.util.matching.Regex

// Bypasses both client and server validation.
object TrustAll extends X509TrustManager {
  val getAcceptedIssuers = null

  override def checkClientTrusted(x509Certificates: Array[X509Certificate], s: String) = {}

  override def checkServerTrusted(x509Certificates: Array[X509Certificate], s: String) = {}
}

// Verifies all host names by simply returning true.
object VerifiesAllHostNames extends HostnameVerifier {
  def verify(s: String, sslSession: SSLSession) = true
}


    class Fetcher() {
        val sslContext = SSLContext.getInstance("SSL")
        sslContext.init(null, Array(TrustAll), new java.security.SecureRandom())
        HttpsURLConnection.setDefaultSSLSocketFactory(sslContext.getSocketFactory)
        HttpsURLConnection.setDefaultHostnameVerifier(VerifiesAllHostNames)

        def fetch(id1:Int,id2:Int): Any={
            //var html:Option[scala.io.BufferedSource] = None
            var out: Any = 0
           // try{

            val url = "https://www.ultimatetennisstatistics.com/h2hProfiles?playerId1="+id1+"&playerId2="+id2
            val html = Source.fromURL(url)
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
            val elos = Array("0","0")
            var i =0
            try{
                for( i <- 1 to 3){
                    elos(0)+= (pattern1 findAllIn textLeft.get(26+i).text()).mkString("").stripPrefix("(").stripSuffix(")")
                }
            }catch{
                    case ex:Exception =>{
                    println("Warning:Player with no ELO score")
                    println(ex)
                    None
                }
            }
            try{
                for( i <- 1 to 3){
                    elos(1)+= (pattern1 findAllIn textRight.get(26+i).text()).mkString("").stripPrefix("(").stripSuffix(")")
                }
            }catch{
                case ex:Exception =>{
                    println("Warning:Player with no ELO score")
                    println(ex)
                    None
                }
            }

            if(elos(0)==0){
                elos(0)="0"
            }
            if(elos(1)==0){
                elos(1)="0"
            }
            println(elos(0)," : "+elos(1))          //punkty elo

            // val elos = Array(
            //     (pattern1 findAllIn textLeft.get(27).text()).mkString("").stripPrefix("(").stripSuffix(")").toInt,
            //     (pattern1 findAllIn textRight.get(27).text()).mkString("").stripPrefix("(").stripSuffix(")").toInt
            //     )
            var surfaceAdjusted:Array[Float] = Array(50,50)
                        try{
             surfaceAdjusted = Array(
                (pattern2 findAllIn document.select("[title='Show H2H matches']").get(0).text()).mkString("").stripSuffix("%").toFloat,
                (pattern2 findAllIn document.select("[title='Show H2H matches']").get(1).text()).mkString("").stripSuffix("%").toFloat
                )
            }catch{
                case ex:Exception =>{
                    println("Warning:Players with no history of matches vs each other")
                    println(ex)
                    None
                }
            }

            

           // println(surfaceAdjusted(0),surfaceAdjusted(1))  //surface adjusted h2h [%]

            var clayMatches:Array[Float] = Array(0,0)
            try{
                    clayMatches = Array(
                    (pattern2 findAllIn document.select("[title='Show clay matches']").get(0).text()).mkString("").stripSuffix("%").toFloat,
                    (pattern2 findAllIn document.select("[title='Show clay matches']").get(1).text()).mkString("").stripSuffix("%").toFloat
                )
            }catch{
                case ex:Exception =>{
                    println("Warning:One or more players has no record of clay matchese")
                    println(ex)
                    None
                }
            }
            var hardMatches:Array[Float] = Array(0,0)
            try{
                    hardMatches = Array(
                    (pattern2 findAllIn document.select("[title='Show hard matches']").get(0).text()).mkString("").stripSuffix("%").toFloat,
                    (pattern2 findAllIn document.select("[title='Show hard matches']").get(1).text()).mkString("").stripSuffix("%").toFloat
                )
            }catch{
                case ex:Exception =>{
                    println("Warning:One or more players has no record of hard matches")
                    println(ex)
                    None
                }
            }
            var carpetMatches:Array[Float] = Array(0,0)
            try{
                carpetMatches = Array(
                (pattern2 findAllIn document.select("[title='Show carpet matches']").get(0).text()).mkString("").stripSuffix("%").toFloat,
                (pattern2 findAllIn document.select("[title='Show carpet matches']").get(1).text()).mkString("").stripSuffix("%").toFloat
            )
            }catch{
                    case ex:Exception =>{
                    println("One or more players has no record of carpet matches")
                    println(ex)
                    None
                }
            }
            var grassMatches:Array[Float] = Array(0,0)
            try{
            grassMatches = Array(
                (pattern2 findAllIn document.select("[title='Show grass matches']").get(0).text()).mkString("").stripSuffix("%").toFloat,
                (pattern2 findAllIn document.select("[title='Show grass matches']").get(1).text()).mkString("").stripSuffix("%").toFloat
            )
            }catch{
                    case ex:Exception =>{
                    println("One or more players has no record of grass matches")
                    println(ex)
                    None
                }
            }
        var bets =Array("","")

        try{
            val betHTML = Source.fromURL("https://matchstat.com/tennis/h2h-odds-bets/"+names(0)+"/"+names(1))
            val betDocument  = Jsoup.parse(betHTML.mkString)

            bets = Array(betDocument.select("span.betrating").get(0).text(),betDocument.select("span.betrating").get(2).text())  //wyniki zakładów
            if(bets(0)==""){    //bez danych zakładów
                bets(0)="0"
            }
            if(bets(1)==""){
                bets(1)="0"
            }
            println(bets(0)," : "+bets(1))         
            }catch{
                case ex:Exception =>{
                    println("Error fetching bets data")
                    println(ex)
                    None
                }  
            }
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
        





            // }catch{
            //     case ex:Exception =>{
            //         println("Error fetching data")
            //         println(ex)
            //         None
            //     }
            // }
            // finally {
            //     return out
            // }
        return out

        }
        def decode(string:String):DisplayPlayer={
            try{
            val html = Source.fromURL("https://www.ultimatetennisstatistics.com/autocompletePlayer?term="+string.replace(' ','+'))
            val data = ujson.read(html.mkString)
            return new DisplayPlayer(data(0)("label").str,data(0)("id").str.toInt)
            }catch{
                case ex:Exception =>{
                    return null
                }
            }

        }
}
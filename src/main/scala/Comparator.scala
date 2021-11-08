package agh
import agh.Surface._

class Comparator(){
    /*
        H2H Surface adjusted		10
        Current Elo			2
        Win%surface			4	
    */
    def compare(players: Array[Int],surface:Surface){
        val fetcher = new Fetcher()
        fetcher.fetch(players(0),players(1))
        for(player <- players){
            //porównanie kazdych dwóch graczy
        }
    }


}
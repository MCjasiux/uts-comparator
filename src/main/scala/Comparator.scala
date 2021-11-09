package agh
import agh.Surface._
import breeze._
import breeze.linalg.{DenseMatrix, DenseVector, padLeft}

class Comparator(){
    /*
        H2H Surface adjusted		10
        Current Elo			2
        Win%surface			4
    */
    //Matrix of weights in pair comparisons between different characteristics
    val characteristicsPairCompMatrix: Array[Array[Double]] = Array(
        Array(1.0,5.0,2.5),
        Array(0.2,1.0,0.5),
        Array(0.4,2.0,1.0)
    )
    val characteristicsMatrix: DenseMatrix[Double] = DenseMatrix(characteristicsPairCompMatrix:_*)
    val saatyValue = 0.52
    normalizeToB(characteristicsPairCompMatrix)
    def compare(players: Array[Int],surface:Surface){
        val fetcher = new Fetcher()
        val fetchMatrix = Array.ofDim[Any](players.length, players.length)
        for {i <- fetchMatrix.indices
             j <- i until fetchMatrix.length
             } {
            fetchMatrix(i)(j) =
                if(i == j) 1
                else fetcher.fetch(players(i), players(j))
        }

        val h2hMatrix = Array.ofDim[Double](players.length, players.length)
        for {i <- h2hMatrix.indices
             j <- i until h2hMatrix.length
             } {
            //h2hMatrix(i)(j) =
            //if(i == j)  1
            //else if (i > j) fetchMatrix(i)(j).h2h
            //else if (i < j) 1/h2hMatrix(j)(i)
        }
        normalizeToB(h2hMatrix)
        val rankMatrix = Array.ofDim[Double](players.length, players.length)
        for {i <- rankMatrix.indices
             j <- i until rankMatrix.length
             } {
            //rankMatrix(i)(j) =
            //if(i == j)  1
            //else if (i > j) rankFunction(fetchMatrix(i)(j).rankPlayer1 - fetchMatrix(i)(j).rankPlayer2)
            //else if (i < j) 1/rankMatrix(j)(i)
        }
        normalizeToB(rankMatrix)
        val winRatioMatrix = Array.ofDim[Double](players.length, players.length)
        for {i <- winRatioMatrix.indices
             j <- i until winRatioMatrix.length
             } {
            //winRatioMatrix(i)(j) =
            //if(i == j)  1
            //else if (i > j) fetchMatrix(i)(j).winRatioPlayer1 / fetchMatrix(i)(j).winRatioPlayer2
            //else if (i < j) 1/winRatioMatrix(j)(i)
        }
        normalizeToB(winRatioMatrix)
        val h2hVector: Array[Double] = toWVector(h2hMatrix)
        //printWithNames(h2hVector)
        calculateCI(h2hVector)
        val rankVector: Array[Double]= toWVector(rankMatrix)
        //printWithNames(rankVector)
        calculateCI(rankVector)
        val winRatioVector: Array[Double] = toWVector(winRatioMatrix)
        //printWithNames(winRatioVector)
        calculateCI(winRatioVector)
        val result = calculateFinalWeights(h2hVector, rankVector, winRatioVector, toWVector(characteristicsPairCompMatrix))


    }
    def rankFunction(rank: Int): Double ={
        if(rank > 0) (scala.math.pow(rank, 1/3)/2) + 0.5
        else 1/(scala.math.pow(scala.math.abs(rank), 1/3)/2) + 0.5
    }
    def normalizeToB(matrix: Array[Array[Double]]): Unit ={
        val accArray = Array[Double](matrix.length)
        for(j <- matrix.indices){
            var acc: Double = 0
            for(i <- matrix.indices){
                acc += matrix(i)(j)
            }
            accArray(j) = acc
        }
        for{
            j <- matrix.indices
            i <- matrix.indices
        }{
            matrix(i)(j) /= accArray(j)
        }
    }
    def toWVector(matrix: Array[Array[Double]]): Array[Double] ={
        for(row <- matrix) yield row.sum/row.length
    }
    def calculateCI(matrix: Array[Double]): Unit ={
        val m = matrix.length
        val vector = DenseVector[Double](matrix:_*)
        val awVector = (characteristicsMatrix*vector).data
        var acc = 0
        for(i <- matrix.indices){
            acc += awVector(i)/matrix(i)
        }
        val maxEigen = acc/m
        println("Max eigenvalue: " + maxEigen)
        val CI = (maxEigen - m)/(m - 1)
        println("CI: " + CI)
        println("RI: " + CI/saatyValue + "%")
    }
    def calculateFinalWeights(h2h: Array[Double], rank: Array[Double], ratio: Array[Double], factorWeights: Array[Double]): Array[Double] ={
        for(i <- h2h.indices.toArray) yield h2h(i)*factorWeights(0) + rank(i)*factorWeights(1) + ratio(i)*factorWeights(2)
    }
    def printWithNames(array: Array[Double]): Unit ={
        //println(names)
        println(array.mkString(" "))
    }

//    def compare2(p1:Player,p2:Player){
//        //porównanie graczy
//    }

}
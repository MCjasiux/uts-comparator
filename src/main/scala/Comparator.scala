package agh
import agh.Surface._
import breeze.linalg.{DenseMatrix, DenseVector}


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
    val characterMatrixB: Array[Array[Double]] = normalizeToB(characteristicsPairCompMatrix)
    def compare(players: Array[Int],surface:Surface){
        val fetcher = new Fetcher()
        val fetchMatrix = Array.ofDim[Any](players.length, players.length)
        for {i <- fetchMatrix.indices
             j <- fetchMatrix.indices
             } {
            if(i ==j )fetchMatrix(i)(j) = 1.0
            else fetchMatrix(i)(j) = fetcher.fetch(players(i), players(j))
        }


        val h2hMatrix = Array.ofDim[Double](players.length, players.length)
        for {i <- h2hMatrix.indices
             j <- h2hMatrix.indices
             } {
            if(i == j) h2hMatrix(i)(j) = 1.0
            else h2hMatrix(i)(j) = fetchMatrix(i)(j).asInstanceOf[H2h].prediction(0) / fetchMatrix(i)(j).asInstanceOf[H2h].prediction(1)
        }
        print("h2h matrix: \n")
        print(h2hMatrix.map(_.mkString(" ")).mkString("\n"))
        print("\n")
        val h2hMatrixB = normalizeToB(h2hMatrix)
        val rankMatrix = Array.ofDim[Double](players.length, players.length)
        for {i <- rankMatrix.indices
             j <- rankMatrix.indices
             } {
            if(i == j)  rankMatrix(i)(j) = 1.0
            else if (i < j) rankMatrix(i)(j) = rankFunction(fetchMatrix(i)(j).asInstanceOf[H2h].elos(0).toDouble - fetchMatrix(i)(j).asInstanceOf[H2h].elos(1).toDouble)
            else  rankMatrix(i)(j) = 1/rankMatrix(j)(i)
        }
        print("elo matrix: \n")
        print(rankMatrix.map(_.mkString(" ")).mkString("\n"))
        print("\n")
        val rankMatrixB = normalizeToB(rankMatrix)
        val winRatioMatrix = Array.ofDim[Double](players.length, players.length)
        for {i <- winRatioMatrix.indices
             j <- winRatioMatrix.indices
             } {

            if(i == j)  winRatioMatrix(i)(j) = 1
            else if (i < j) winRatioMatrix(i)(j) = fetchMatrix(i)(j).asInstanceOf[H2h].surfaceWins(surface)(0) / fetchMatrix(i)(j).asInstanceOf[H2h].surfaceWins(surface)(1)
            else winRatioMatrix(i)(j) =  1/winRatioMatrix(j)(i)
        }
        print("winRatio matrix: \n")
        print(winRatioMatrix.map(_.mkString(" ")).mkString("\n"))
        print("\n")
        val winRatioMatrixB = normalizeToB(winRatioMatrix)
        val names = getNames(fetchMatrix)
        val h2hVector: Array[Double] = toWVector(h2hMatrixB)
        print("h2h comparison: \n")
        printWithNames(h2hVector, names)
        calculateCI(h2hVector, h2hMatrix)
        print("\n")
        val rankVector: Array[Double]= toWVector(rankMatrixB)
        print("elo comparison: \n")
        printWithNames(rankVector, names)
        calculateCI(rankVector, rankMatrix)
        print("\n")
        val winRatioVector: Array[Double] = toWVector(winRatioMatrixB)
        print("winRatio comparison: \n")
        printWithNames(winRatioVector, names)
        calculateCI(winRatioVector, winRatioMatrix)
        print("\n")
        val result = calculateFinalWeights(h2hVector, rankVector, winRatioVector, toWVector(characterMatrixB))
        print("Final comparison: \n")
        printWithNames(result, names)
        print("\n")
        print("Our choice: " + names(result.indexOf(result.max)))
    }
    def getNames(fetchMatrix: Array[Array[Any]]): Array[String] ={
        val result = Array.ofDim[String](fetchMatrix.length)
        var split = fetchMatrix(1)(0).asInstanceOf[H2h].name.split("vs")
        result(0) = split(1)
        for (i <- 1 until fetchMatrix.length){
            split = fetchMatrix(0)(i).asInstanceOf[H2h].name.split("vs")
            result(i) = split(1)
        }

        result
    }

    def rankFunction(rank: Double): Double ={
        if(rank > 0) ((scala.math.cbrt(rank)/4) + 0.75)
        else 1/((scala.math.cbrt(scala.math.abs(rank))/4) + 0.75)
    }
    def normalizeToB(matrix: Array[Array[Double]]): Array[Array[Double]] ={
        val accArray = Array.ofDim[Double](matrix.length)
        val result = Array.ofDim[Double](matrix.length, matrix(0).length)
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
            result(i)(j) = matrix(i)(j) / accArray(j)
        }
        result
    }
    def toWVector(matrix: Array[Array[Double]]): Array[Double] ={
        for(row <- matrix) yield row.sum/row.length
    }
    def calculateCI(matrix: Array[Double], A: Array[Array[Double]]): Unit ={
        val m = matrix.length
        val vector = DenseVector[Double](matrix:_*)
        val aDense = DenseMatrix(A:_*)
        val awVector = (aDense*vector).data
        var acc: Double = 0
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
    def printWithNames(array: Array[Double], names: Array[String]): Unit ={
        println(names.mkString(" "))
        println(array.mkString(" "))
    }


//    def compare2(p1:Player,p2:Player){
//        //porównanie graczy
//    }

}
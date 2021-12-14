package agh
import agh.Surface._
import breeze.linalg.{DenseMatrix, DenseVector}


class Comparator(){
    /*
        H2H Surface adjusted		10
        Current Elo			2
        Win%surface			4
        bet             5
    */
    //Matrix of weights in pair comparisons between different characteristics
    val characteristicsPairCompMatrix: Array[Array[Double]] = Array(
        Array(1.0,5.0,2.5,2.0),
        Array(0.2,1.0,0.5,0.6),
        Array(0.4,2.0,1.0,0.8),
        Array(0.5,1.666666667,1.25,1.0),
    )
    val characteristicsMatrix: DenseMatrix[Double] = DenseMatrix(characteristicsPairCompMatrix:_*)
    val saatyValues: Array[Double] = Array(0.0, 0.0, 0.58, 0.90, 1.12)
    val characterMatrixB: Array[Array[Double]] = normalizeToB(characteristicsPairCompMatrix)
    def fetchData(players: Array[Int],surface:Surface): Array[Array[Any]] ={
        val fetcher = new Fetcher()
        val fetchMatrix = Array.ofDim[Any](players.length, players.length)
        for {i <- fetchMatrix.indices
             j <- fetchMatrix.indices
             } {
            if(i ==j )fetchMatrix(i)(j) = 1.0
            else fetchMatrix(i)(j) = fetcher.fetch(players(i), players(j))
        }
        fetchMatrix
    }
    def fetchMatrixes(fetchMatrix: Array[Array[Any]], players: Array[Int],surface:Surface): Array[Array[Array[Double]]]={

        val h2hMatrix = Array.ofDim[Double](players.length, players.length)
        for {i <- h2hMatrix.indices
             j <- h2hMatrix.indices
             } {
            if(i == j) h2hMatrix(i)(j) = 1.0
            else h2hMatrix(i)(j) = fetchMatrix(i)(j).asInstanceOf[H2h].prediction(0) / fetchMatrix(i)(j).asInstanceOf[H2h].prediction(1)
        }
        print("\n")
        print("h2h matrix: \n")
        print(h2hMatrix.map(_.mkString(" ")).mkString("\n"))
        print("\n")
        val rankMatrix = Array.ofDim[Double](players.length, players.length)
        for {i <- rankMatrix.indices
             j <- rankMatrix.indices
             } {
            if(i == j)  rankMatrix(i)(j) = 1.0
            else if (i < j) rankMatrix(i)(j) = rankFunction(fetchMatrix(i)(j).asInstanceOf[H2h].elos(0).toDouble - fetchMatrix(i)(j).asInstanceOf[H2h].elos(1).toDouble)
            else  rankMatrix(i)(j) = 1/rankMatrix(j)(i)
        }
        print("\n")
        print("elo matrix: \n")
        print(rankMatrix.map(_.mkString(" ")).mkString("\n"))
        print("\n")

        val winRatioMatrix = Array.ofDim[Double](players.length, players.length)
        for {i <- winRatioMatrix.indices
             j <- winRatioMatrix.indices
             } {

            if(i == j)  winRatioMatrix(i)(j) = 1
            else if (i < j) winRatioMatrix(i)(j) = fetchMatrix(i)(j).asInstanceOf[H2h].surfaceWins(surface)(0) / fetchMatrix(i)(j).asInstanceOf[H2h].surfaceWins(surface)(1)
            else winRatioMatrix(i)(j) =  1/winRatioMatrix(j)(i)
        }
        print("\n")
        print("winRatio matrix: \n")
        print(winRatioMatrix.map(_.mkString(" ")).mkString("\n"))
        print("\n")

        print("\n")

        val betMatrix = Array.ofDim[Double](players.length, players.length)
        for {i <- betMatrix.indices
             j <- betMatrix.indices
             } {
            if(i == j)  betMatrix(i)(j) = 1.0
            else if (i < j) betMatrix(i)(j) = betFunction(fetchMatrix(i)(j).asInstanceOf[H2h].bets(0).toDouble - fetchMatrix(i)(j).asInstanceOf[H2h].bets(1).toDouble)
            else  betMatrix(i)(j) = 1/betMatrix(j)(i)
        }
        print("\n")
        print("bet matrix: \n")
        print(betMatrix.map(_.mkString(" ")).mkString("\n"))
        print("\n")
        Array(h2hMatrix, rankMatrix, winRatioMatrix, betMatrix)
    }

    def compareEVM(players: Array[Int], surface:Surface){
        val fetchMatrix = fetchData(players, surface)
        val arrs = fetchMatrixes(fetchMatrix, players, surface)
        val h2hMatrix = arrs(0)
        val rankMatrix = arrs(1)
        val winRatioMatrix = arrs(2)
        val betMatrix = arrs(3)
        val names = getNames(fetchMatrix)
        val h2hMatrixB = normalizeToB(h2hMatrix)
        val rankMatrixB = normalizeToB(rankMatrix)
        val winRatioMatrixB = normalizeToB(winRatioMatrix)
        val betMatrixB = normalizeToB(betMatrix)

        val h2hVector: Array[Double] = toWVector(h2hMatrixB)
        print("h2h comparison: \n")
        printWithNames(h2hVector, names)
        calculateCI(h2hVector, h2hMatrix)
        koczkodajIndex(h2hMatrix.length, h2hMatrix)

        print("\n")

        val rankVector: Array[Double]= toWVector(rankMatrixB)
        print("elo comparison: \n")
        printWithNames(rankVector, names)
        calculateCI(rankVector, rankMatrix)
        koczkodajIndex(rankMatrix.length, rankMatrix)

        print("\n")

        val winRatioVector: Array[Double] = toWVector(winRatioMatrixB)
        print("winRatio comparison: \n")
        printWithNames(winRatioVector, names)
        calculateCI(winRatioVector, winRatioMatrix)
        koczkodajIndex(winRatioMatrix.length, winRatioMatrix)
        print("\n")

        val betVector: Array[Double]= toWVector(betMatrixB)
        print("bet comparison: \n")
        printWithNames(betVector, names)
        calculateCI(betVector, betMatrix)
        koczkodajIndex(betMatrix.length, betMatrix)
        print("\n")

        val result = calculateFinalWeights(h2hVector, rankVector, winRatioVector,betVector,  toWVector(characterMatrixB))
        print("Final comparison: \n")
        printWithNames(result, names)
        print("\n")
        print("Our choice: " + names(result.indexOf(result.max)))
    }
    def compareGMM(players: Array[Int],surface:Surface): Unit ={
        val fetchMatrix = fetchData(players, surface)
        val arrs = fetchMatrixes(fetchMatrix, players, surface)
        val h2hMatrix = arrs(0)
        val rankMatrix = arrs(1)
        val winRatioMatrix = arrs(2)
        val betMatrix = arrs(3)
        val names = getNames(fetchMatrix)
        val h2hVector = toGMMVector(h2hMatrix)
        val rankVector = toGMMVector(rankMatrix)
        val winRatioVector = toGMMVector(winRatioMatrix)
        val betVector = toGMMVector(betMatrix)

        val h2hVectorN: Array[Double] = normalizeVector(h2hVector)
        print("h2h comparison: \n")
        printWithNames(h2hVectorN, names)
        calculateCIGMM(h2hVector)
        koczkodajIndex(h2hMatrix.length, h2hMatrix)
        print("\n")

        val rankVectorN: Array[Double]= normalizeVector(rankVector)
        print("elo comparison: \n")
        printWithNames(rankVectorN, names)
        calculateCIGMM(rankVector)
        koczkodajIndex(rankMatrix.length, rankMatrix)
        print("\n")

        val winRatioVectorN: Array[Double] = normalizeVector(winRatioVector)
        print("winRatio comparison: \n")
        printWithNames(winRatioVectorN, names)
        calculateCIGMM(winRatioVector)
        koczkodajIndex(winRatioMatrix.length, winRatioMatrix)

        print("\n")

        val betVectorN: Array[Double]= normalizeVector(betVector)
        print("bet comparison: \n")
        printWithNames(betVectorN, names)
        calculateCIGMM(betVector)
        koczkodajIndex(betMatrix.length, betMatrix)

        print("\n")

        val result = calculateFinalWeights(h2hVectorN, rankVectorN, winRatioVectorN,betVectorN, normalizeVector(toGMMVector(characteristicsPairCompMatrix)))
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
    def betFunction(diff: Double): Double ={
        if(diff > 0) 0.04*diff + 1
        else 1/(0.04*math.abs(diff) + 1)
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
    def normalizeVector(vector: Array[Double]): Array[Double] ={
        var accArray: Double = 0
        vector.foreach(accArray += _)
        val res = for (x <- vector) yield x/accArray
        res
    }
    def toGMMVector(matrix: Array[Array[Double]]): Array[Double]={
        val acc = Array.fill[Double](matrix.length)(1)
        for (i <- matrix.indices){
            matrix(i).foreach(acc(i) *= _)
        }
        val res = for (x <- acc) yield nthRoot(x, matrix.length)
        res
    }

    def nthRoot(A: Double, N: Double): Double = {
        var xPre = Math.random % 10
        val eps = 0.001
        var delX: Double = 2147483647
        var xK = 0.0
        while ( {
            delX > eps
        }) {
            xK = ((N - 1.0) * xPre + A / Math.pow(xPre, N - 1)) / N
            delX = Math.abs(xK - xPre)
            xPre = xK
        }
        xK
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
        println("RI: " + CI*100/saatyValues(characteristicsPairCompMatrix.length) + "%")
    }
    def calculateCIGMM(vector: Array[Double]): Unit ={
        val m = vector.length
        var acc: Double = 0.0
        vector.foreach(acc += _)
        val maxEigen = acc
        println("Max eigenvalue: " + maxEigen)
        val CI = (maxEigen - m)/(m - 1)
        println("CI: " + CI)
        println("RI: " + CI*100/saatyValues(characteristicsPairCompMatrix.length) + "%")
    }
    def permutations(n: Int): Int = {
        var acc = 1
        for (i <- 1 to n){
            acc*=i
        }
        acc
    }

    def combinations(n: Int, k: Int): Int =
        permutations(n) / (permutations(k) * permutations(n - k))


    def koczkodajIndex(nrOfAlternatives: Int,  A: Array[Array[Double]]): Unit ={
        val triads = Array.ofDim[Array[Int]](combinations(nrOfAlternatives, 3))
        var index = 0
        for(i <- 0 to triads.length-3){
            for(j<- i + 1 to triads.length-2){
                for(k <- j+1 to triads.length-1){
                    triads(index) = Array[Int](i,j,k)
                    index+=1
                }
            }
        }
        val triadInconsistencies = Array.ofDim[Double](combinations(nrOfAlternatives, 3))
        for (i <- triadInconsistencies.indices){
            triadInconsistencies(i) = math.min(math.abs(1-(A(triads(i)(0))(triads(i)(2))*A(triads(i)(2))(triads(i)(1))/A(triads(i)(0))(triads(i)(1)))),math.abs(1-(A(triads(i)(0))(triads(i)(1))/A(triads(i)(0))(triads(i)(2))*A(triads(i)(2))(triads(i)(1)))))
        }
        val koczkodajIndex = triadInconsistencies.max
        println("koczkodajIndex: " + koczkodajIndex)
        val limit = 1/(1- koczkodajIndex) -1
        println("CI limit: " + limit)
    }
    def calculateFinalWeights(h2h: Array[Double], rank: Array[Double], ratio: Array[Double],bet: Array[Double], factorWeights: Array[Double]): Array[Double] ={
        for(i <- h2h.indices.toArray) yield h2h(i)*factorWeights(0) + rank(i)*factorWeights(1) + ratio(i)*factorWeights(2) + bet(i)*factorWeights(3)
    }
    def printWithNames(array: Array[Double], names: Array[String]): Unit ={
        println(names.mkString(" "))
        println(array.mkString(" "))
    }


//    def compare2(p1:Player,p2:Player){
//        //porównanie graczy
//    }

}
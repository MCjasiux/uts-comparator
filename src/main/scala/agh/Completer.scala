package agh

import breeze.linalg.{DenseMatrix, DenseVector}
import Math.log
import Math.exp
import Array._

class Completer(){

    def complete(matrix: Array[Array[Double]]):Array[Array[Double]]={
        var g:Array[Array[Double]] = ofDim[Double](matrix.length,matrix.length)
        
        var y = 0;
        for ( i <-matrix ) {
            var x = 0
            var missing = 0
            for ( j <- i ) {
                if(j==0){
                    g(y)(x)=1
                    missing +=1
                }else{
                    g(y)(x)=0 
                }
                x+=1
            }
            g(y)(y)= matrix.length - missing
            y+=1
      }
      var r:Array[Double] = ofDim[Double](matrix.length)
      var index=0
      for(i <- matrix){
          var temp:Double = 0
          for(j <- i){
              if(j!=0){
                  temp += log(j)
              }
          }
          r(index)=temp
          index+=1
      }

   // print(g.map(_.mkString(" ")).mkString("\n")+"\n")
   // print(r.map(_.toString.mkString(" ")).mkString("\n")+"\n")
    var w:Array[Double] = ofDim[Double](matrix.length)
    index=0
    for( i <- g){
        var indey=0
        w(index)=0
        for(j<-i){
            w(index) += r(indey)*j
            indey+=1
        }
        index+=1
    }
    var trueW:Array[Double] = ofDim[Double](matrix.length)
    trueW = w.map(exp(_))
    var sum = 0.0
        for(i<-w){
            sum+=i
        }
    var finalW:Array[Double] = ofDim[Double](matrix.length)
    finalW = trueW.map(_/sum)
  //  print(finalW.map(_.toString.mkString(" ")).mkString("\n")+"\n")

    var out:Array[Array[Double]] = ofDim[Double](matrix.length,matrix.length)
    
    var indey=0
    for(i <- matrix){
        index=0
        for(j <-i){
            if(j==0){
                out(indey)(index)=finalW(indey)/finalW(index)
            }else{
                out(indey)(index)=j
            }
            index+=1
        }
    indey+=1
    }
  //  print(out.map(_.mkString(" ")).mkString("\n")+"\n")
    return out
    }

}
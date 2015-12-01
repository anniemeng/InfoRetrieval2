package TermBasedModel

import collection.mutable.LinkedHashMap
import scala.collection.mutable

class TFIDFScore {
  
  // input: tf(w,d), df(w), n
  def score(tfs: mutable.LinkedHashMap[String, Map[String, Int]],
            dfs: mutable.LinkedHashMap[String, Map[String, Int]],
            docsN: Int): LinkedHashMap[String, Double] = {
    
    // val rdfs =
    
    var lm = mutable.LinkedHashMap[String, Double]()
    for ((q, m) <- tfs) {
      var sumTFIDF = 0.0
      for ((k, f) <- m) {
        val mult = log2(1+f) * log2( docsN / dfs(q).getOrElse(k, 0).toDouble )
        sumTFIDF += mult
      }
      lm(q) = sumTFIDF
    }
    lm
  }
  
  def log2 (x: Double) = scala.math.log10(x) / scala.math.log10(2.0)
  
}

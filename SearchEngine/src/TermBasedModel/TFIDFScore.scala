package TermBasedModel

import collection.mutable.LinkedHashMap
import scala.collection.mutable

class TFIDFScore {
  
  // input: tf(w,d), df(w)
  def score(tfs: mutable.LinkedHashMap[String, Map[String, Int]],
            dfs: mutable.LinkedHashMap[String, Map[String, Int]]
            /*,docsL: Int*/) {
    
    // val rdfs =
    
    var lm = mutable.LinkedHashMap[String, Int]()
    for ((q, m) <- tfs) {
      var sumTFIDF = 0
      for ((k, f) <- m) {
        val mult = f * dfs(q).getOrElse(k, 0)
        sumTFIDF += mult
      }
      lm(q) = sumTFIDF
    }
    
  }

}
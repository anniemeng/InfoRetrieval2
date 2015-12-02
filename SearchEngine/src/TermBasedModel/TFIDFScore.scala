package TermBasedModel

import main._

import collection.mutable.LinkedHashMap
import scala.collection.mutable

object TFIDFScore {
  
  // input: tf(w,d), df(w), n, doc_id
  def score(tfs: mutable.LinkedHashMap[String, mutable.Map[String, Int]],
            dfs: mutable.LinkedHashMap[String, mutable.LinkedHashMap[String, Int]],
            docsN: Int,
            doc_id: String) {
    
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
    
    var counter = 0
    for ((q, score) <- lm) {
      // update the min heap
      val heap_size = main.minHeapsTerm(counter).size
      if (heap_size < 100) {
        main.minHeapsTerm(counter) += Tuple2(score, doc_id)
      } else {
        val testScore = main.minHeapsTerm(counter).head._1
        if (score > testScore) {
          main.minHeapsTerm(counter) += Tuple2(score,doc_id)
          main.minHeapsTerm(counter).dequeue()
        }
      }
      counter += 1
    }
  }
  
  def log2 (x: Double) = scala.math.log10(x) / scala.math.log10(2.0)
  
}

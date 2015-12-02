package TermBasedModel

import main._

import collection.mutable.LinkedHashMap
import scala.collection.mutable

object TFScore {

  // input: term frequencies, sum of terms in document, doc_id
  def score(tfs: mutable.LinkedHashMap[String, mutable.Map[String, Double]],
      tfsSum: Double,
      doc_id: String) {
    
    // transform to log-term frequencies
    tfs.mapValues(m => m.mapValues { v => log2( (v.toDouble+1.0) / tfsSum ) })
    
    // sum over all query terms = score of documents
    val scores = tfs.map{ case (q, m) => (q, m.foldLeft(0.0)(_+_._2)) }
    
    var counter = 0
    for ((q, score) <- scores) {
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
